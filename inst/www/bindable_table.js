/**
 * Bindable Table Input Widget JavaScript
 * Author: Jay Warrick / Salus Team
 * Description: Custom Shiny InputBinding for schema-driven interactive tables
 * supporting contiguous grouping, repeats, inline cell edits, drag-and-drop reorder,
 * and high-fidelity reactive synchronization.
 */

(function() {
  'use strict';

  // State registry for active widget instances
  const instances = new Map();

  class BindableTableInstance {
    constructor(element) {
      this.el = element;
      this.id = element.getAttribute('data-input-id');
      const allowAttr = element.getAttribute('data-allow-grouping') ?? element.getAttribute('data-enable-groups');
      this.enableGroups = (allowAttr === null || allowAttr === 'true');
      this.container = element.querySelector('.bindable-table-container');

      // Load schema and initial data from embedded JSON tags
      const schemaScript = element.querySelector('.bindable-table-schema');
      const dataScript = element.querySelector('.bindable-table-initial-data');

      this.schema = schemaScript ? JSON.parse(schemaScript.textContent) : {};
      this.rows = dataScript ? this.normalizeRows(JSON.parse(dataScript.textContent)) : [];

      this.selectedIndices = new Set();
      this.dragStartIndex = null;
      this.isDragging = false;
      this.isMouseDownSelecting = false;
      this.selectionAnchorIndex = null;
      this.shinyCallback = null;

      this.initDOM();
      this.render();
      this.attachEvents();
    }

    getNextStepId() {
      let maxId = 0;
      for (const r of this.rows) {
        const sid = parseInt(r._step_id, 10);
        if (!isNaN(sid) && sid > maxId) maxId = sid;
      }
      return maxId + 1;
    }

    getNextGroupId() {
      let maxGid = 0;
      for (const r of this.rows) {
        const gid = parseInt(r._group_id, 10);
        if (!isNaN(gid) && gid > maxGid) maxGid = gid;
      }
      return maxGid + 1;
    }

    normalizeRows(raw) {
      if (!Array.isArray(raw)) {
        // If passed as column-oriented object from R data.frame
        if (typeof raw === 'object' && raw !== null) {
          const colKeys = Object.keys(raw);
          if (colKeys.length === 0) return [];
          const numRows = Array.isArray(raw[colKeys[0]]) ? raw[colKeys[0]].length : 0;
          const result = [];
          for (let r = 0; r < numRows; r++) {
            const rowObj = {};
            for (const k of colKeys) {
              rowObj[k] = raw[k][r];
            }
            result.push(rowObj);
          }
          return this.sanitizeRowObjects(result);
        }
        return [];
      }
      return this.sanitizeRowObjects(raw);
    }

    sanitizeRowObjects(rowsList) {
      return rowsList.map((row, idx) => {
        const item = Object.assign({}, row);
        item._step_id = parseInt(item._step_id, 10) || (idx + 1);
        const parsedGid = parseInt(item._group_id, 10);
        item._group_id = (!isNaN(parsedGid) && parsedGid > 0) ? parsedGid : null;
        item._repeats = parseInt(item._repeats, 10) || 1;
        if (item._repeats < 1) item._repeats = 1;

        // Ensure schema defaults
        for (const colId of Object.keys(this.schema)) {
          const colDef = this.schema[colId];
          if (item[colId] === undefined || item[colId] === null) {
            item[colId] = colDef.default !== undefined ? colDef.default : '';
          }
          if (colDef.type === 'logical') {
            item[colId] = Boolean(item[colId]);
          } else if (colDef.type === 'numeric') {
            item[colId] = (item[colId] === '' || isNaN(item[colId])) ? (colDef.default || 0) : Number(item[colId]);
          } else if (colDef.type === 'character' && colId === 'name' && !item[colId]) {
            item[colId] = 'Step ' + (idx + 1);
          }
        }
        return item;
      });
    }

    initDOM() {
      this.container.innerHTML = `
        <div class="bindable-table-toolbar">
          <div class="bindable-table-toolbar-left">
            <button type="button" class="btn btn-default btn-sm bindable-btn" data-action="add-row">
              + Add Row
            </button>
            ${this.enableGroups ? `
              <button type="button" class="btn btn-default btn-sm bindable-btn" data-action="group-rows" disabled title="Group selected rows and specify repeats">
                Group & Repeat
              </button>
              <button type="button" class="btn btn-default btn-sm bindable-btn" data-action="ungroup-rows" disabled title="Ungroup selected grouped rows">
                Ungroup
              </button>
            ` : ''}
            <button type="button" class="btn btn-default btn-sm bindable-btn" data-action="duplicate-row" disabled title="Duplicate selected row">
              Duplicate
            </button>
            <button type="button" class="btn btn-default btn-sm bindable-btn text-danger" data-action="delete-rows" disabled title="Delete selected row(s)">
              Delete
            </button>
          </div>
        </div>
        <div class="bindable-table-scroll">
          <table class="bindable-table">
            <thead>
              <tr data-role="header-row"></tr>
            </thead>
            <tbody data-role="body"></tbody>
          </table>
        </div>
      `;

      this.headerRowEl = this.container.querySelector('[data-role="header-row"]');
      this.tbodyEl = this.container.querySelector('[data-role="body"]');

      // Build table header
      let thHtml = `
        <th class="bindable-col-handle" title="Drag to reorder"></th>
      `;

      for (const colId of Object.keys(this.schema)) {
        const col = this.schema[colId];
        const widthStyle = col.width && col.width !== 'auto' ? `style="width:${col.width};"` : '';
        thHtml += `<th ${widthStyle}>${this.escapeHtml(col.name)}</th>`;
      }

      if (this.enableGroups) {
        thHtml += `<th class="bindable-col-repeat">Repeats</th>`;
      }

      this.headerRowEl.innerHTML = thHtml;
    }

    render() {
      if (this.rows.length === 0) {
        const totalCols = Object.keys(this.schema).length + (this.enableGroups ? 2 : 1);
        this.tbodyEl.innerHTML = `
          <tr>
            <td colspan="${totalCols}" class="bindable-empty-state">
              No rows defined. Click <strong>+ Add Row</strong> to get started.
            </td>
          </tr>
        `;
        this.updateToolbarState();
        return;
      }

      // Identify groups and compute span information
      const groupSpans = new Map(); // group_id -> { startIndex, endIndex, count, repeats }
      if (this.enableGroups) {
        let currentGid = null;
        let startIdx = 0;
        let groupCount = 0;

        for (let i = 0; i < this.rows.length; i++) {
          const gid = this.rows[i]._group_id;
          if (gid) {
            if (gid === currentGid) {
              groupCount++;
            } else {
              if (currentGid) {
                groupSpans.set(currentGid, {
                  startIndex: startIdx,
                  endIndex: i - 1,
                  count: groupCount,
                  repeats: this.rows[startIdx]._repeats || 1
                });
              }
              currentGid = gid;
              startIdx = i;
              groupCount = 1;
            }
          } else {
            if (currentGid) {
              groupSpans.set(currentGid, {
                startIndex: startIdx,
                endIndex: i - 1,
                count: groupCount,
                repeats: this.rows[startIdx]._repeats || 1
              });
              currentGid = null;
            }
          }
        }
        if (currentGid) {
          groupSpans.set(currentGid, {
            startIndex: startIdx,
            endIndex: this.rows.length - 1,
            count: groupCount,
            repeats: this.rows[startIdx]._repeats || 1
          });
        }
      }
      this.groupSpans = groupSpans;

      let html = '';
      let unitIndex = 0;
      let lastGid = null;

      for (let i = 0; i < this.rows.length; i++) {
        const row = this.rows[i];
        const isSelected = this.selectedIndices.has(i);
        const isDisabled = row.use === false;
        const gid = row._group_id;
        const isGrouped = Boolean(this.enableGroups && gid);

        // Logical unit alternating shading: rows in the same group share the exact same shade
        if (isGrouped) {
          if (gid !== lastGid) {
            unitIndex++;
            lastGid = gid;
          }
        } else {
          unitIndex++;
          lastGid = null;
        }

        const shadeClass = (unitIndex % 2 === 1) ? 'unit-shade-a' : 'unit-shade-b';
        let rowClasses = ['bindable-row', shadeClass];
        if (isSelected) rowClasses.push('row-selected');
        if (isGrouped) rowClasses.push('is-grouped');
        if (isDisabled) rowClasses.push('row-disabled');

        html += `<tr class="${rowClasses.join(' ')}" data-index="${i}" data-step-id="${row._step_id}" ${isGrouped ? `data-group-id="${gid}"` : ''}>`;

        // 1. Drag Handle Column (Texture dots vertically centered in merged cell for groups)
        if (isGrouped) {
          const gInfo = groupSpans.get(gid);
          if (gInfo && gInfo.startIndex === i) {
            html += `
              <td class="bindable-col-handle bindable-group-handle" rowspan="${gInfo.count}" title="Drag group to reorder (${gInfo.count} rows)">
                <div class="bindable-drag-handle" draggable="true" data-index="${i}" data-group-id="${gid}">
                  <span class="bindable-drag-dots">⋮⋮</span>
                </div>
              </td>
            `;
          }
          // Subsequent rows in group: omit <td> so rowspan occupies the handle column
        } else {
          html += `
            <td class="bindable-col-handle">
              <div class="bindable-drag-handle" draggable="true" data-index="${i}" title="Drag row to reorder">
                <span class="bindable-drag-dots">⋮⋮</span>
              </div>
            </td>
          `;
        }

        // 2. Schema-Defined Columns
        for (const colId of Object.keys(this.schema)) {
          const colDef = this.schema[colId];
          const val = row[colId] !== undefined ? row[colId] : colDef.default;

          if (colDef.type === 'logical') {
            html += `
              <td class="bindable-cell-use">
                <input type="checkbox" class="bindable-use-toggle" data-col="${colId}" data-index="${i}" ${val ? 'checked' : ''} />
              </td>
            `;
          } else if (colDef.type === 'numeric') {
            const minAttr = colDef.min !== undefined && colDef.min !== null ? `min="${colDef.min}"` : '';
            const maxAttr = colDef.max !== undefined && colDef.max !== null ? `max="${colDef.max}"` : '';
            const stepAttr = colDef.step ? `step="${colDef.step}"` : 'step="0.1"';
            html += `
              <td>
                <input type="number" class="bindable-input bindable-input-number" data-col="${colId}" data-index="${i}"
                       value="${Number(val)}" ${minAttr} ${maxAttr} ${stepAttr} />
              </td>
            `;
          } else if (colDef.type === 'select' && Array.isArray(colDef.choices)) {
            let opts = colDef.choices.map(c => `<option value="${this.escapeHtml(c)}" ${String(val) === String(c) ? 'selected' : ''}>${this.escapeHtml(c)}</option>`).join('');
            html += `
              <td>
                <select class="bindable-input" data-col="${colId}" data-index="${i}">
                  ${opts}
                </select>
              </td>
            `;
          } else {
            // Default character/text input
            const placeholder = colId === 'name' ? `Row ${i + 1}` : '';
            html += `
              <td>
                <input type="text" class="bindable-input" data-col="${colId}" data-index="${i}"
                       value="${this.escapeHtml(String(val || ''))}" placeholder="${placeholder}" />
              </td>
            `;
          }
        }

        // 4. Repeat Group Column (With Rowspan for Group Blocks)
        if (this.enableGroups) {
          if (gid) {
            const gInfo = groupSpans.get(gid);
            if (gInfo && gInfo.startIndex === i) {
              // First row of group: render merged cell spanning all rows in this group
              // Clean repeat box: NO circular arrow, NO 'x' after number, clean '✕' ungroup button
              html += `
                <td class="bindable-col-repeat" rowspan="${gInfo.count}">
                  <div class="bindable-group-bracket-box">
                    <span class="bindable-repeat-badge">Repeat:</span>
                    <input type="number" class="bindable-repeat-input" data-group-id="${gid}"
                           value="${gInfo.repeats}" min="1" max="999" step="1" title="Repetitions" />
                    <button type="button" class="bindable-btn-ungroup" data-action="ungroup-id" data-group-id="${gid}" title="Ungroup this block">
                      ✕
                    </button>
                  </div>
                </td>
              `;
            }
            // Non-first rows of group: rowspan covers it, do not render <td>
          } else {
            // Standalone single row: completely blank cell (no label text)
            html += `<td class="bindable-col-repeat"></td>`;
          }
        }

        html += `</tr>`;
      }

      this.tbodyEl.innerHTML = html;
      this.updateToolbarState();
    }

    updateToolbarState() {
      const selCount = this.selectedIndices.size;

      // Update button disabled states
      const btnGroup = this.container.querySelector('[data-action="group-rows"]');
      const btnUngroup = this.container.querySelector('[data-action="ungroup-rows"]');
      const btnDup = this.container.querySelector('[data-action="duplicate-row"]');
      const btnDel = this.container.querySelector('[data-action="delete-rows"]');

      if (btnDup) btnDup.disabled = (selCount === 0);
      if (btnDel) btnDel.disabled = (selCount === 0);

      if (btnGroup) {
        btnGroup.disabled = !this.canGroupSelection();
      }
      if (btnUngroup) {
        btnUngroup.disabled = !this.canUngroupSelection();
      }
    }

    canGroupSelection() {
      if (this.selectedIndices.size < 2) return false;
      const sorted = Array.from(this.selectedIndices).sort((a, b) => a - b);

      // 1. Must be contiguous
      for (let i = 0; i < sorted.length - 1; i++) {
        if (sorted[i + 1] !== sorted[i] + 1) return false;
      }

      // 2. Non-overlapping rule: No partial intersection with an existing group
      const minIdx = sorted[0];
      const maxIdx = sorted[sorted.length - 1];

      // Check if any existing group spans outside this selection
      for (let i = 0; i < this.rows.length; i++) {
        const gid = this.rows[i]._group_id;
        if (gid) {
          const isInSelection = (i >= minIdx && i <= maxIdx);
          // Find full extent of this group
          let gStart = i;
          while (gStart > 0 && this.rows[gStart - 1]._group_id === gid) gStart--;
          let gEnd = i;
          while (gEnd < this.rows.length - 1 && this.rows[gEnd + 1]._group_id === gid) gEnd++;

          // If the group partially overlaps the selection boundary, grouping is illegal
          const groupOverlapsBoundary = (gStart < minIdx && gEnd >= minIdx) || (gEnd > maxIdx && gStart <= maxIdx);
          if (groupOverlapsBoundary) return false;
        }
      }

      return true;
    }

    canUngroupSelection() {
      if (this.selectedIndices.size === 0) return false;
      for (const idx of this.selectedIndices) {
        if (this.rows[idx] && this.rows[idx]._group_id) return true;
      }
      return false;
    }

    attachEvents() {
      // Toolbar action delegation
      this.container.addEventListener('click', (e) => {
        const btn = e.target.closest('[data-action]');
        if (!btn) return;
        const action = btn.getAttribute('data-action');

        if (action === 'add-row') {
          this.addRow();
        } else if (action === 'group-rows') {
          this.groupSelectedRows();
        } else if (action === 'ungroup-rows') {
          this.ungroupSelectedRows();
        } else if (action === 'ungroup-id') {
          const gid = btn.getAttribute('data-group-id');
          this.ungroupByGroupId(gid);
        } else if (action === 'duplicate-row') {
          this.duplicateSelectedRow();
        } else if (action === 'delete-rows') {
          this.deleteSelectedRows();
        }
      });

      // Shift-click / Drag-Select Range on Rows
      this.container.addEventListener('mousedown', (e) => {
        const row = e.target.closest('.bindable-row');
        if (!row || e.target.closest('input') || e.target.closest('select') || e.target.closest('button')) return;
        const idx = parseInt(row.getAttribute('data-index'), 10);

        if (e.shiftKey && this.selectionAnchorIndex !== null) {
          // Select range from anchor to clicked
          const start = Math.min(this.selectionAnchorIndex, idx);
          const end = Math.max(this.selectionAnchorIndex, idx);
          this.selectedIndices.clear();
          for (let k = start; k <= end; k++) this.selectedIndices.add(k);
          this.render();
        } else if (e.metaKey || e.ctrlKey) {
          if (this.selectedIndices.has(idx)) this.selectedIndices.delete(idx);
          else this.selectedIndices.add(idx);
          this.selectionAnchorIndex = idx;
          this.render();
        } else {
          // Normal click on row selects just that row
          this.selectedIndices.clear();
          this.selectedIndices.add(idx);
          this.selectionAnchorIndex = idx;
          this.render();
        }
      });

      // Inline Cell Editing (use-toggle, text, number, select)
      this.container.addEventListener('change', (e) => {
        const target = e.target;
        if (target.classList.contains('bindable-use-toggle')) {
          const idx = parseInt(target.getAttribute('data-index'), 10);
          const col = target.getAttribute('data-col');
          if (this.rows[idx]) {
            this.rows[idx][col] = target.checked;
            this.render();
            this.notifyShiny();
          }
        } else if (target.classList.contains('bindable-input')) {
          const idx = parseInt(target.getAttribute('data-index'), 10);
          const col = target.getAttribute('data-col');
          if (this.rows[idx]) {
            const colDef = this.schema[col];
            if (colDef && colDef.type === 'numeric') {
              this.rows[idx][col] = Number(target.value);
            } else {
              this.rows[idx][col] = target.value;
            }
            this.updateToolbarState();
            this.notifyShiny();
          }
        } else if (target.classList.contains('bindable-repeat-input')) {
          const gid = target.getAttribute('data-group-id');
          const reps = Math.max(1, parseInt(target.value, 10) || 1);
          target.value = reps;
          for (const r of this.rows) {
            if (r._group_id === gid) r._repeats = reps;
          }
          this.updateToolbarState();
          this.notifyShiny();
        }
      });

      // HTML5 Drag and Drop Row & Group Reordering
      this.container.addEventListener('dragstart', (e) => {
        const handle = e.target.closest('.bindable-drag-handle');
        if (!handle) {
          e.preventDefault();
          return;
        }

        const startIdx = parseInt(handle.getAttribute('data-index'), 10);
        if (isNaN(startIdx) || !this.rows[startIdx]) {
          e.preventDefault();
          return;
        }

        this.dragStartIndex = startIdx;
        this.isDragging = true;
        this.currentDropDestIndex = null;

        const sourceRow = this.rows[startIdx];
        const gid = sourceRow._group_id;

        // If dragging a group, visually dim all rows in this group
        if (gid) {
          this.container.querySelectorAll(`.bindable-row[data-group-id="${gid}"]`).forEach(r => r.classList.add('is-dragging'));
        } else {
          const row = handle.closest('.bindable-row');
          if (row) row.classList.add('is-dragging');
        }

        e.dataTransfer.effectAllowed = 'move';
        e.dataTransfer.setData('text/plain', String(this.dragStartIndex));
      });

      this.container.addEventListener('dragover', (e) => {
        if (!this.isDragging) return;
        e.preventDefault();
        e.dataTransfer.dropEffect = 'move';

        const targetRow = e.target.closest('.bindable-row');
        if (!targetRow) return;

        const targetIndex = parseInt(targetRow.getAttribute('data-index'), 10);
        if (isNaN(targetIndex) || !this.rows[targetIndex]) return;

        this.clearDragHoverClasses();

        const sourceRow = this.rows[this.dragStartIndex];
        const sourceGid = sourceRow ? sourceRow._group_id : null;
        const targetRowObj = this.rows[targetIndex];
        const targetGid = targetRowObj ? targetRowObj._group_id : null;

        let destIndex = null;
        let highlightRow = null;
        let highlightPos = null;

        if (targetGid && this.groupSpans && this.groupSpans.has(targetGid)) {
          // Target row is inside a group:
          // CANNOT insert into the middle of an existing group!
          // Must snap to either TOP of entire group or BOTTOM of entire group!
          const gInfo = this.groupSpans.get(targetGid);
          const firstRowEl = this.tbodyEl.querySelector(`.bindable-row[data-index="${gInfo.startIndex}"]`);
          const lastRowEl = this.tbodyEl.querySelector(`.bindable-row[data-index="${gInfo.endIndex}"]`);

          if (firstRowEl && lastRowEl) {
            const firstRect = firstRowEl.getBoundingClientRect();
            const lastRect = lastRowEl.getBoundingClientRect();
            const groupMidY = (firstRect.top + lastRect.bottom) / 2;

            if (e.clientY < groupMidY) {
              destIndex = gInfo.startIndex;
              highlightRow = firstRowEl;
              highlightPos = 'drag-over-top';
            } else {
              destIndex = gInfo.endIndex + 1;
              highlightRow = lastRowEl;
              highlightPos = 'drag-over-bottom';
            }
          }
        } else {
          // Target row is a standalone ungrouped row
          const rect = targetRow.getBoundingClientRect();
          const midY = rect.top + rect.height / 2;
          if (e.clientY < midY) {
            destIndex = targetIndex;
            highlightRow = targetRow;
            highlightPos = 'drag-over-top';
          } else {
            destIndex = targetIndex + 1;
            highlightRow = targetRow;
            highlightPos = 'drag-over-bottom';
          }
        }

        // Compute bounds of the source block being dragged
        let sourceStart = this.dragStartIndex;
        let sourceEnd = this.dragStartIndex;
        if (sourceGid && this.groupSpans && this.groupSpans.has(sourceGid)) {
          const sgInfo = this.groupSpans.get(sourceGid);
          sourceStart = sgInfo.startIndex;
          sourceEnd = sgInfo.endIndex;
        }

        // Disallow dropping into self (no-op)
        if (destIndex >= sourceStart && destIndex <= sourceEnd + 1) {
          this.currentDropDestIndex = null;
          return;
        }

        this.currentDropDestIndex = destIndex;
        if (highlightRow && highlightPos) {
          highlightRow.classList.add(highlightPos);
        }
      });

      this.container.addEventListener('dragleave', (e) => {
        const related = e.relatedTarget;
        if (!related || !this.tbodyEl.contains(related)) {
          this.clearDragHoverClasses();
        }
      });

      this.container.addEventListener('drop', (e) => {
        if (!this.isDragging) return;
        e.preventDefault();
        const destIndex = this.currentDropDestIndex;
        const srcIndex = this.dragStartIndex;
        this.cleanupDrag();

        if (destIndex !== null && destIndex !== undefined && !isNaN(destIndex)) {
          this.moveRowOrGroup(srcIndex, destIndex);
        }
      });

      this.container.addEventListener('dragend', () => {
        this.cleanupDrag();
      });
    }

    clearDragHoverClasses() {
      this.tbodyEl.querySelectorAll('.bindable-row').forEach(r => {
        r.classList.remove('drag-over-top', 'drag-over-bottom');
      });
    }

    cleanupDrag() {
      this.isDragging = false;
      this.dragStartIndex = null;
      this.currentDropDestIndex = null;
      this.container.querySelectorAll('.bindable-row').forEach(r => {
        r.classList.remove('is-dragging', 'drag-over-top', 'drag-over-bottom');
      });
    }

    moveRowOrGroup(sourceIdx, destIdx) {
      const sourceRow = this.rows[sourceIdx];
      if (!sourceRow) return;

      const gid = sourceRow._group_id;
      let startIdx = sourceIdx;
      let endIdx = sourceIdx;

      // If moving a grouped row, move the ENTIRE group together
      if (gid) {
        while (startIdx > 0 && this.rows[startIdx - 1]._group_id === gid) startIdx--;
        while (endIdx < this.rows.length - 1 && this.rows[endIdx + 1]._group_id === gid) endIdx++;
      }

      // If destination is within or directly adjacent to moving range, no-op
      if (destIdx >= startIdx && destIdx <= endIdx + 1) return;

      const count = (endIdx - startIdx) + 1;
      const movingBlock = this.rows.splice(startIdx, count);

      let adjustedDest = destIdx;
      if (destIdx > startIdx) {
        adjustedDest -= count;
      }
      if (adjustedDest < 0) adjustedDest = 0;
      if (adjustedDest > this.rows.length) adjustedDest = this.rows.length;

      this.rows.splice(adjustedDest, 0, ...movingBlock);

      // Re-select moved rows
      this.selectedIndices.clear();
      for (let k = 0; k < count; k++) {
        this.selectedIndices.add(adjustedDest + k);
      }

      this.render();
      this.notifyShiny();
    }

    addRow() {
      const newRow = { _step_id: this.getNextStepId(), _group_id: null, _repeats: 1 };
      for (const colId of Object.keys(this.schema)) {
        const colDef = this.schema[colId];
        newRow[colId] = colDef.default;
        if (colId === 'name') {
          newRow[colId] = `Step ${this.rows.length + 1}`;
        }
      }

      // If a row is currently selected, insert below it (or below its entire group); otherwise append to end
      let insertAt = this.rows.length;
      if (this.selectedIndices.size > 0) {
        const sorted = Array.from(this.selectedIndices).sort((a, b) => b - a);
        const topSelectedIdx = sorted[0];
        const selGid = this.rows[topSelectedIdx] ? this.rows[topSelectedIdx]._group_id : null;
        if (selGid && this.groupSpans && this.groupSpans.has(selGid)) {
          insertAt = this.groupSpans.get(selGid).endIndex + 1;
        } else {
          insertAt = topSelectedIdx + 1;
        }
      }

      this.rows.splice(insertAt, 0, newRow);
      this.selectedIndices.clear();
      this.selectedIndices.add(insertAt);
      this.render();
      this.notifyShiny();
    }

    duplicateSelectedRow() {
      if (this.selectedIndices.size === 0) return;

      const sortedIndices = Array.from(this.selectedIndices).sort((a, b) => a - b);

      if (this.enableGroups) {
        // Check if all selected indices belong to the exact same group
        const firstRow = this.rows[sortedIndices[0]];
        const rawGid = firstRow ? firstRow._group_id : null;
        const isGrouped = rawGid !== null && rawGid !== undefined && rawGid !== '';

        if (isGrouped) {
          const gid = String(rawGid);
          const allInSameGroup = sortedIndices.every(i => String(this.rows[i]._group_id) === gid);

          if (allInSameGroup) {
            // Find full group boundaries in the table
            let grpStart = sortedIndices[0];
            while (grpStart > 0 && String(this.rows[grpStart - 1]._group_id) === gid) {
              grpStart--;
            }
            let grpEnd = sortedIndices[sortedIndices.length - 1];
            while (grpEnd < this.rows.length - 1 && String(this.rows[grpEnd + 1]._group_id) === gid) {
              grpEnd++;
            }
            const fullGrpIndices = [];
            for (let i = grpStart; i <= grpEnd; i++) fullGrpIndices.push(i);

            const isEntireGroupSelected = (sortedIndices.length === fullGrpIndices.length) &&
              sortedIndices.every((val, idx) => val === fullGrpIndices[idx]);

            // Case A: Entire group is selected -> duplicate the whole group as a NEW group
            if (isEntireGroupSelected) {
              const newGid = this.getNextGroupId();
              let nextStepId = this.getNextStepId();
              const clonedGroup = fullGrpIndices.map(i => {
                const row = this.rows[i];
                const clone = Object.assign({}, row);
                clone._step_id = nextStepId++;
                clone._group_id = newGid;
                clone._repeats = row._repeats;
                if (clone.name) clone.name = clone.name + ' (Copy)';
                return clone;
              });

              this.rows.splice(grpEnd + 1, 0, ...clonedGroup);
              this.selectedIndices.clear();
              for (let i = 0; i < clonedGroup.length; i++) {
                this.selectedIndices.add(grpEnd + 1 + i);
              }
              this.render();
              this.notifyShiny();
              return;
            }

            // Case B: A single row or subset within the group is selected -> duplicate WITHIN the group
            const lastSelectedIdx = sortedIndices[sortedIndices.length - 1];
            let nextStepId = this.getNextStepId();
            const clonedRows = sortedIndices.map(i => {
              const row = this.rows[i];
              const clone = Object.assign({}, row);
              clone._step_id = nextStepId++;
              clone._group_id = row._group_id; // Preserve group membership
              clone._repeats = row._repeats;   // Preserve group repeat count
              if (clone.name) clone.name = clone.name + ' (Copy)';
              return clone;
            });

            this.rows.splice(lastSelectedIdx + 1, 0, ...clonedRows);
            this.selectedIndices.clear();
            for (let i = 0; i < clonedRows.length; i++) {
              this.selectedIndices.add(lastSelectedIdx + 1 + i);
            }
            this.render();
            this.notifyShiny();
            return;
          }
        }
      }

      // Default / Ungrouped duplication (or when enableGroups is false)
      const lastSelectedIdx = sortedIndices[sortedIndices.length - 1];
      let nextStepId = this.getNextStepId();
      const clonedRows = sortedIndices.map(i => {
        const row = this.rows[i];
        const clone = Object.assign({}, row);
        clone._step_id = nextStepId++;
        clone._group_id = null;
        clone._repeats = 1;
        if (clone.name) clone.name = clone.name + ' (Copy)';
        return clone;
      });

      this.rows.splice(lastSelectedIdx + 1, 0, ...clonedRows);
      this.selectedIndices.clear();
      for (let i = 0; i < clonedRows.length; i++) {
        this.selectedIndices.add(lastSelectedIdx + 1 + i);
      }
      this.render();
      this.notifyShiny();
    }

    deleteSelectedRows() {
      if (this.selectedIndices.size === 0) return;
      const sorted = Array.from(this.selectedIndices).sort((a, b) => b - a);
      for (const idx of sorted) {
        this.rows.splice(idx, 1);
      }
      this.selectedIndices.clear();
      this.render();
      this.notifyShiny();
    }

    groupSelectedRows() {
      if (!this.canGroupSelection()) return;
      const sorted = Array.from(this.selectedIndices).sort((a, b) => a - b);
      const newGid = this.getNextGroupId();
      const defaultRepeats = 2;

      for (const idx of sorted) {
        this.rows[idx]._group_id = newGid;
        this.rows[idx]._repeats = defaultRepeats;
      }

      this.render();
      this.notifyShiny();
    }

    ungroupSelectedRows() {
      if (this.selectedIndices.size === 0) return;
      for (const idx of this.selectedIndices) {
        if (this.rows[idx]) {
          this.rows[idx]._group_id = null;
          this.rows[idx]._repeats = 1;
        }
      }
      this.render();
      this.notifyShiny();
    }

    ungroupByGroupId(gid) {
      if (!gid && gid !== 0) return;
      const targetGid = parseInt(gid, 10);
      for (const r of this.rows) {
        if (r._group_id === targetGid || String(r._group_id) === String(gid)) {
          r._group_id = null;
          r._repeats = 1;
        }
      }
      this.render();
      this.notifyShiny();
    }

    notifyShiny() {
      if (typeof this.shinyCallback === 'function') {
        this.shinyCallback(true);
      }
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(this.id, this.getValue(), { priority: 'event' });
      }
      $(this.el).trigger('change');
    }

    setAllowGrouping(allowed) {
      this.enableGroups = Boolean(allowed);
      if (!this.enableGroups) {
        // Clear grouping info so all steps are treated as single steps
        for (const r of this.rows) {
          r._group_id = null;
          r._repeats = 1;
        }
      }
      this.initDOM();
      this.render();
      this.notifyShiny();
    }

    getValue() {
      // Serialize clean data object with an explicit revision timestamp
      // so Shiny's reactiveValues detects the change and invalidates dependents 100% reliably
      return {
        rows: this.rows.map(r => Object.assign({}, r)),
        _rev: Date.now()
      };
    }

    setValue(newVal) {
      this.rows = this.normalizeRows(newVal);
      this.selectedIndices.clear();
      this.render();
      this.notifyShiny();
    }

    escapeHtml(str) {
      if (str === null || str === undefined) return '';
      return String(str)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
    }
  }

  // Define Shiny Custom Input Binding
  const bindableTableBinding = new Shiny.InputBinding();

  $.extend(bindableTableBinding, {
    find: function(scope) {
      return $(scope).find('.bindable-table-widget');
    },
    getType: function(el) {
      return 'bindableWidgets.table';
    },
    initialize: function(el) {
      const instance = new BindableTableInstance(el);
      instances.set(el, instance);
      $(el).data('bindableTableInstance', instance);
      window.__bindableTableInstances = window.__bindableTableInstances || {};
      window.__bindableTableInstances[el.id || el.getAttribute('data-input-id')] = instance;
    },
    getId: function(el) {
      return el.getAttribute('data-input-id') || el.id;
    },
    getValue: function(el) {
      const instance = instances.get(el);
      if (!instance) return null;
      return instance.getValue();
    },
    setValue: function(el, value) {
      const instance = instances.get(el);
      if (instance) instance.setValue(value);
    },
    subscribe: function(el, callback) {
      const instance = instances.get(el);
      if (instance) {
        instance.shinyCallback = callback;
      }
      $(el).on('change.bindableTableBinding', function() {
        callback(true);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.bindableTableBinding');
      const instance = instances.get(el);
      if (instance) instance.shinyCallback = null;
      instances.delete(el);
    }
  });

  Shiny.inputBindings.register(bindableTableBinding, 'salus.bindableTable');

  // Handle server-side custom message to update widget
  Shiny.addCustomMessageHandler('bindableTable_setValue', function(message) {
    if (message && message.id) {
      const widgetEl = document.getElementById(message.id);
      if (widgetEl) {
        const instance = instances.get(widgetEl);
        if (instance) {
          instance.setValue(message.rows || message.value);
        }
      }
    }
  });

  // Handle server-side custom message to dynamically toggle grouping
  Shiny.addCustomMessageHandler('bindableTable_setAllowGrouping', function(message) {
    if (message && message.id) {
      const widgetEl = document.getElementById(message.id);
      if (widgetEl) {
        const instance = instances.get(widgetEl);
        if (instance) {
          instance.setAllowGrouping(message.allow_grouping);
        }
      }
    }
  });

  if (typeof window !== 'undefined') {
    window.BindableTableInstance = BindableTableInstance;
  }
  if (typeof global !== 'undefined') {
    global.BindableTableInstance = BindableTableInstance;
  }

})();
