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
import com.logicaldoc.gui.common.client.widgets.FolderChangeListener;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DropMoveEvent;
import com.smartgwt.client.widgets.events.DropMoveHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Shows the set of filters associated to the curent account
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountFiltersPanel extends EmailAccountDetailsTab {

	private ListGrid list;

	public EmailAccountFiltersPanel(GUIEmailAccount account, ChangedHandler changedHandler) {
		super(account, changedHandler);
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		IButton addRule = new IButton(I18N.message("addrule"));
		addRule.setHeight(25);
		addRule.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				ListGridRecord record = new ListGridRecord();
				record.setAttribute("field", "0");
				record.setAttribute("condition", "0");
				record.setAttribute("expression", "");
				record.setAttribute("targetId", "5");
				record.setAttribute("targetName", "/");
				list.addData(record);
				EmailAccountFiltersPanel.this.changedHandler.onChanged(null);
			}
		});

		HLayout formsContainer = new HLayout();

		ListGridField field = new ListGridField("field", I18N.message("field"));
		field.setWidth(120);
		SelectItem fieldSelect = ItemFactory.newEmailFields("field", "field");
		field.setEditorType(fieldSelect);
		fieldSelect.addChangedHandler(changedHandler);
		field.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if ("0".equals(value))
					return I18N.message("subject");
				else if ("1".equals(value))
					return I18N.message("sender");
				else if ("2".equals(value))
					return I18N.message("content");
				else
					return I18N.message("recipient");
			}
		});

		ListGridField condition = new ListGridField("condition", I18N.message("condition"));
		condition.setWidth(120);
		SelectItem conditionSelect = new SelectItem();
		LinkedHashMap<String, String> map2 = new LinkedHashMap<String, String>();
		map2.put("0", I18N.message("contains"));
		map2.put("1", I18N.message("notcontains"));
		map2.put("2", I18N.message("matches"));
		conditionSelect.setValueMap(map2);
		condition.setEditorType(conditionSelect);
		conditionSelect.addChangedHandler(changedHandler);
		condition.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if ("0".equals(value))
					return I18N.message("contains");
				else if ("1".equals(value))
					return I18N.message("notcontains");
				else
					return I18N.message("matches");
			}
		});

		ListGridField expression = new ListGridField("expression", I18N.message("expression"));
		expression.setWidth(150);
		TextItem conditionEdit = ItemFactory.newTextItem("expression", "expression", null);
		expression.setEditorType(conditionEdit);
		conditionEdit.addChangedHandler(changedHandler);

		final ListGridField target = new ListGridField("targetName", I18N.message("targetfolder"));
		target.setWidth(150);
		target.setCanEdit(false);
		target.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return "";
			}
		});

		list = new ListGrid() {
			@Override
			protected Canvas createRecordComponent(final ListGridRecord record, Integer colNum) {
				String fieldName = this.getFieldName(colNum);
				if (fieldName.equals("targetName")) {
					final FolderSelector folderSelector = new FolderSelector("target", true);
					folderSelector.setShowTitle(false);
					folderSelector.setFolder(Long.parseLong(record.getAttributeAsString("targetId")),
							record.getAttributeAsString("targetName"));
					folderSelector.addFolderChangeListener(new FolderChangeListener() {

						@Override
						public void onChanged(GUIFolder folder) {
							if (folderSelector.getFolderId() != null) {
								record.setAttribute("targetId", Long.toString(folderSelector.getFolderId()));
								record.setAttribute("targetName", folderSelector.getFolderName());
							} else {
								record.setAttribute("targetId", (Long) null);
								record.setAttribute("targetName", "");
							}

							changedHandler.onChanged(null);
						}
					});

					DynamicForm l = new DynamicForm();
					l.setFields(folderSelector);
					return l;
				} else {
					return super.createRecordComponent(record, colNum);
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

		List<ListGridRecord> records = new ArrayList<ListGridRecord>();
		if (account.getRules() != null)
			for (GUIEmailRule rule : account.getRules()) {
				ListGridRecord record = new ListGridRecord();
				record.setAttribute("field", Integer.toString(rule.getField()));
				record.setAttribute("condition", Integer.toString(rule.getPolicy()));
				record.setAttribute("expression", rule.getExpression());
				record.setAttribute("targetId", Long.toString(rule.getTarget().getId()));
				record.setAttribute("targetName", rule.getTarget().getName());
				records.add(record);
			}
		list.setData(records.toArray(new ListGridRecord[0]));

		list.setFields(field, condition, expression, target);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		list.addDropMoveHandler(new DropMoveHandler() {
			@Override
			public void onDropMove(DropMoveEvent event) {
				changedHandler.onChanged(null);
			}
		});

		list.addEditCompleteHandler(new EditCompleteHandler() {
			@Override
			public void onEditComplete(EditCompleteEvent event) {
				changedHandler.onChanged(null);
			}
		});

		formsContainer.setMembers(list);
		setMembers(formsContainer, addRule);
	}

	boolean validate() {
		if (list == null)
			return true;

		ListGridRecord[] records = list.getRecords();
		List<GUIEmailRule> rules = new ArrayList<GUIEmailRule>();
		for (ListGridRecord record : records) {
			if (record.getAttribute("expression") == null)
				continue;
			GUIEmailRule rule = new GUIEmailRule();
			GUIFolder target = new GUIFolder();

			if (record.getAttributeAsLong("targetId") != null)
				target.setId(record.getAttributeAsLong("targetId"));
			else
				target.setId(0L);

			target.setName(record.getAttribute("targetName"));
			rule.setTarget(target);
			rule.setField(Integer.parseInt(record.getAttribute("field")));
			rule.setPolicy(Integer.parseInt(record.getAttribute("condition")));
			rule.setExpression(record.getAttribute("expression"));
			rules.add(rule);
		}

		account.setRules(rules.toArray(new GUIEmailRule[0]));
		return true;
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							list.removeSelectedData();
							changedHandler.onChanged(null);
						}
					}
				});
			}
		});

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}
}