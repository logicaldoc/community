package com.logicaldoc.gui.frontend.client.impex.email;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.beans.GUIEmailRule;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Shows the set of filters associated to the curent account
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountFiltersPanel extends EmailAccountDetailsTab {

	private static final String TARGET_NAME = "targetName";

	private static final String TARGET_ID = "targetId";

	private static final String CONDITION = "condition";

	private static final String FIELD = "field";

	private static final String EXPRESSION = "expression";

	private ListGrid list;

	public EmailAccountFiltersPanel(GUIEmailAccount account, ChangedHandler changedHandler) {
		super(account, changedHandler);
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		IButton addRule = prepareAddRuleButton();

		HLayout formsContainer = new HLayout();

		ListGridField field = prepareFieldField();

		ListGridField condition = prepareConditionField();

		ListGridField expression = new ListGridField(EXPRESSION, I18N.message(EXPRESSION));
		expression.setWidth(150);
		TextItem conditionEdit = ItemFactory.newTextItem(EXPRESSION, null);
		expression.setEditorType(conditionEdit);
		conditionEdit.addChangedHandler(changedHandler);

		final ListGridField target = prepareTargetField();

		prepareList(field);

		List<ListGridRecord> records = new ArrayList<>();
		if (account.getRules() != null)
			for (GUIEmailRule rule : account.getRules()) {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute(FIELD, Integer.toString(rule.getField()));
				rec.setAttribute(CONDITION, Integer.toString(rule.getPolicy()));
				rec.setAttribute(EXPRESSION, rule.getExpression());
				rec.setAttribute(TARGET_ID, Long.toString(rule.getTarget().getId()));
				rec.setAttribute(TARGET_NAME, rule.getTarget().getName());
				records.add(rec);
			}
		list.setData(records.toArray(new ListGridRecord[0]));

		list.setFields(field, condition, expression, target);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addDropMoveHandler(event -> changedHandler.onChanged(null));

		list.addEditCompleteHandler(event -> changedHandler.onChanged(null));

		formsContainer.setMembers(list);
		setMembers(formsContainer, addRule);
	}

	private void prepareList(ListGridField field) {
		list = new ListGrid() {
			@Override
			protected Canvas createRecordComponent(final ListGridRecord rec, Integer colNum) {
				String fieldName = this.getFieldName(colNum);
				if (fieldName.equals(TARGET_NAME)) {
					final FolderSelector folderSelector = new FolderSelector("target", true);
					folderSelector.setShowTitle(false);
					folderSelector.setFolder(Long.parseLong(rec.getAttributeAsString(TARGET_ID)),
							rec.getAttributeAsString(TARGET_NAME));
					folderSelector.addFolderChangeListener((GUIFolder folder) -> {
						if (folderSelector.getFolderId() != null) {
							rec.setAttribute(TARGET_ID, Long.toString(folderSelector.getFolderId()));
							rec.setAttribute(TARGET_NAME, folderSelector.getFolderName());
						} else {
							rec.setAttribute(TARGET_ID, (Long) null);
							rec.setAttribute(TARGET_NAME, "");
						}

						changedHandler.onChanged(null);
					});

					DynamicForm l = new DynamicForm();
					l.setFields(folderSelector);
					return l;
				} else {
					return super.createRecordComponent(rec, colNum);
				}
			}
		};
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setCanEdit(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(field);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setShowAllRecords(true);
		list.setCanReorderRecords(true);
		list.setCanDragRecordsOut(true);
		list.setCanAcceptDroppedRecords(true);
		list.setDragDataAction(DragDataAction.MOVE);
		list.setModalEditing(true);
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
	}

	private ListGridField prepareTargetField() {
		final ListGridField target = new ListGridField(TARGET_NAME, I18N.message("targetfolder"));
		target.setWidth(150);
		target.setCanEdit(false);
		target.setCellFormatter((value, rec, rowNum, colNum) -> {
			return "";
		});
		return target;
	}

	private ListGridField prepareConditionField() {
		ListGridField condition = new ListGridField(CONDITION, I18N.message(CONDITION));
		condition.setWidth(120);
		SelectItem conditionSelect = new SelectItem();
		LinkedHashMap<String, String> map2 = new LinkedHashMap<>();
		map2.put("0", I18N.message("contains"));
		map2.put("1", I18N.message("notcontains"));
		map2.put("2", I18N.message("matches"));
		conditionSelect.setValueMap(map2);
		condition.setEditorType(conditionSelect);
		conditionSelect.addChangedHandler(changedHandler);
		condition.setCellFormatter((value, rec, rowNum, colNum) -> {
			if ("0".equals(value))
				return I18N.message("contains");
			else if ("1".equals(value))
				return I18N.message("notcontains");
			else
				return I18N.message("matches");
		});
		return condition;
	}

	private ListGridField prepareFieldField() {
		ListGridField field = new ListGridField(FIELD, I18N.message(FIELD));
		field.setWidth(120);
		SelectItem fieldSelect = ItemFactory.newEmailFields(FIELD, FIELD);
		field.setEditorType(fieldSelect);
		fieldSelect.addChangedHandler(changedHandler);
		field.setCellFormatter((value, rec, rowNum, colNum) -> {
			if ("0".equals(value))
				return I18N.message("subject");
			else if ("1".equals(value))
				return I18N.message("sender");
			else if ("2".equals(value))
				return I18N.message("content");
			else
				return I18N.message("recipient");
		});
		return field;
	}

	private IButton prepareAddRuleButton() {
		IButton addRule = new IButton(I18N.message("addrule"));
		addRule.setHeight(25);
		addRule.addClickHandler(addRuleClick -> {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(FIELD, "0");
			rec.setAttribute(CONDITION, "0");
			rec.setAttribute(EXPRESSION, "");
			rec.setAttribute(TARGET_ID, "5");
			rec.setAttribute(TARGET_NAME, "/");
			list.addData(rec);
			EmailAccountFiltersPanel.this.changedHandler.onChanged(null);
		});
		return addRule;
	}

	boolean validate() {
		try {
			if (list != null) {
				ListGridRecord[] records = list.getRecords();
				List<GUIEmailRule> rules = new ArrayList<>();
				for (ListGridRecord rec : records) {
					if (rec.getAttribute(EXPRESSION) == null)
						continue;
					GUIEmailRule rule = new GUIEmailRule();
					GUIFolder target = new GUIFolder();

					if (rec.getAttributeAsLong(TARGET_ID) != null)
						target.setId(rec.getAttributeAsLong(TARGET_ID));
					else
						target.setId(0L);

					target.setName(rec.getAttribute(TARGET_NAME));
					rule.setTarget(target);
					rule.setField(Integer.parseInt(rec.getAttribute(FIELD)));
					rule.setPolicy(Integer.parseInt(rec.getAttribute(CONDITION)));
					rule.setExpression(rec.getAttribute(EXPRESSION));
					rules.add(rule);
				}

				account.setRules(rules.toArray(new GUIEmailRule[0]));
			}
			return true;
		} catch (Exception t) {
			return false;
		}
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				list.removeSelectedData();
				changedHandler.onChanged(null);
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}
}