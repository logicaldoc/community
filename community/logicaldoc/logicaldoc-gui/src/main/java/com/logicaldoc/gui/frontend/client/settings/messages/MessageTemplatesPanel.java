package com.logicaldoc.gui.frontend.client.settings.messages;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the patterns for generated messages.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class MessageTemplatesPanel extends VLayout {

	private ListGrid list;

	private SelectItem langSelector;

	public MessageTemplatesPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	public void onDraw() {
		HTMLFlow hint = new HTMLFlow(I18N.message("messagetemplatehint"));
		hint.setMargin(3);

		langSelector = ItemFactory.newLanguageSelector("language", false, true);
		langSelector.setWrapTitle(false);
		langSelector.setMultiple(false);
		langSelector.setEndRow(false);
		langSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				reload();
			}
		});

		ToolStripButton add = new ToolStripButton();
		add.setAutoFit(true);
		add.setTitle(I18N.message("addtemplate"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				langSelector.setValue("en");
				TextItem item = ItemFactory.newSimpleTextItem("name", "", null);
				item.setRequired(true);
				LD.askforValue(I18N.message("newtemplate"), I18N.message("name"), null, item, new ValueCallback() {
					@Override
					public void execute(String value) {
						ListGridRecord record = new ListGridRecord();
						record.setAttribute("id", "-1");
						record.setAttribute("type", "user");
						record.setAttribute("name", value);
						list.getRecordList().addAt(record, 0);
						list.startEditing(0);
					}
				});
			}
		});

		ToolStripButton save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				saveTemplates();
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addFormItem(langSelector);
		toolStrip.addButton(add);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
		toolStrip.addFill();
		toolStrip.setWidth100();

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth(80);
		name.setRequired(true);
		name.setCanEdit(false);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setWidth(80);
		type.setCanEdit(false);

		ListGridField subject = new ListGridField("subject", I18N.message("subject"));
		subject.setWidth(160);
		subject.setRequired(false);

		ListGridField body = new ListGridField("body", I18N.message("body"));
		body.setWidth("*");
		body.setRequired(false);
		body.setEditorType(new TextAreaItem());

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setCanEdit(true);
		list.setWidth100();
		list.setHeight100();
		list.setFields(name);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setModalEditing(true);
		list.setFields(name, type, subject, body);
		list.setCanReorderRecords(true);
		list.setCanDragRecordsOut(true);
		list.setCanAcceptDroppedRecords(true);
		list.setDragDataAction(DragDataAction.MOVE);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		// Initialize with english language
		langSelector.setValue("en");

		setMembers(hint, toolStrip, list);

		reload();
	}

	/**
	 * Sends the patterns in the grid to the server to save them.
	 */
	private void saveTemplates() {
		String lang = langSelector.getValueAsString();
		if (lang == null || lang.isEmpty())
			return;

		Record[] records = list.getRecords();
		GUIMessageTemplate[] templates = new GUIMessageTemplate[records.length];
		int i = 0;
		for (Record record : records) {
			GUIMessageTemplate t = new GUIMessageTemplate();
			t.setId(new Long(record.getAttributeAsString("id")));
			t.setLanguage(lang);
			t.setName(record.getAttributeAsString("name"));
			t.setSubject(record.getAttributeAsString("subject"));
			t.setBody(record.getAttributeAsString("body"));
			t.setType(record.getAttributeAsString("type"));

			templates[i++] = t;
		}

		MessageService.Instance.get().saveTemplates(templates, new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				Log.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem copyFromDefault = new MenuItem();
		copyFromDefault.setTitle(I18N.message("copyfromdefault"));
		copyFromDefault.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord[] records = list.getSelectedRecords();
				long[] ids = new long[records.length];
				for (int i = 0; i < records.length; i++) {
					// Avoid deletion of default templates
					if (!"en".equals(records[i].getAttributeAsString("language")))
						ids[i] = Long.parseLong(records[i].getAttributeAsString("id"));
				}

				MessageService.Instance.get().deleteTemplates(ids, new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						reload();
					}
				});
			}
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				MessageService.Instance.get().deleteTemplates(list.getSelectedRecord().getAttributeAsString("name"),
						new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {
								reload();
							}
						});
			}
		});

		delete.setEnabled(!"system".equals(list.getSelectedRecord().getAttributeAsString("type")));
		copyFromDefault.setEnabled(!"en".equals(langSelector.getValueAsString()));

		contextMenu.setItems(copyFromDefault, delete);
		contextMenu.showContextMenu();
	}

	private void reload() {
		String lang = langSelector.getValueAsString();

		MessageService.Instance.get().loadTemplates(lang, null, new AsyncCallback<GUIMessageTemplate[]>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIMessageTemplate[] templates) {
				ListGridRecord[] records = new ListGridRecord[templates.length];
				int i = 0;
				for (GUIMessageTemplate pat : templates) {
					ListGridRecord record = new ListGridRecord();
					record.setAttribute("id", pat.getId());
					record.setAttribute("name", pat.getName());
					record.setAttribute("language", pat.getName());
					record.setAttribute("subject", pat.getSubject());
					record.setAttribute("body", pat.getBody());
					record.setAttribute("type", pat.getType());
					records[i++] = record;
				}
				list.setData(records);
			}
		});
	}
}