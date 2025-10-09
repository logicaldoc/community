package com.logicaldoc.gui.frontend.client.settings.messages;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
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

	private static final String SUBJECT = "subject";

	private static final String LANGUAGE = "language";

	private ListGrid list;

	private HLayout rollOverCanvas;

	private ListGridRecord rollOverRecord;

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

		langSelector = ItemFactory.newLanguageSelector(LANGUAGE, false, true);
		langSelector.setWrapTitle(false);
		langSelector.setMultiple(false);
		langSelector.setEndRow(false);
		langSelector.addChangedHandler((ChangedEvent event) -> reload());

		ToolStripButton add = new ToolStripButton();
		add.setAutoFit(true);
		add.setTitle(I18N.message("addmessagetemplate"));
		add.addClickHandler((ClickEvent event) -> {
			langSelector.setValue("en");
			TextItem item = ItemFactory.newSimpleTextItem("name", "", null);
			item.setRequired(true);
			LD.askForValue(I18N.message("newmessagetemplate"), I18N.message("name"), null, item, value -> {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute("id", "-1");
				rec.setAttribute("type", "user");
				rec.setAttribute("name", value);
				list.getRecordList().addAt(rec, 0);
				list.startEditing(0);
			});
		});

		ToolStripButton save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> saveTemplates());

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addFormItem(langSelector);
		toolStrip.addButton(add);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
		toolStrip.addFill();
		toolStrip.setWidth100();

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setAutoFitWidth(true);
		name.setRequired(true);
		name.setCanEdit(false);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setWidth(60);
		type.setCanEdit(false);

		ListGridField subject = new ListGridField(SUBJECT, I18N.message(SUBJECT));
		subject.setWidth(160);
		subject.setRequired(false);

		ListGridField body = new ListGridField("body", I18N.message("body"));
		body.setWidth("*");
		body.setRequired(false);

		list = new ListGrid() {
			@Override
			protected Canvas getRollOverCanvas(Integer rowNum, Integer colNum) {
				rollOverRecord = this.getRecord(rowNum);

				if (rollOverCanvas == null) {
					rollOverCanvas = new HLayout(3);
					rollOverCanvas.setSnapTo("R");
					rollOverCanvas.setWidth(50);
					rollOverCanvas.setHeight(22);

					ImgButton editImg = new ImgButton();
					editImg.setShowDown(false);
					editImg.setShowRollOver(false);
					editImg.setLayoutAlign(Alignment.CENTER);
					editImg.setSrc("[SKIN]/icons/pencil.png");
					editImg.setPrompt(I18N.message("edit"));
					editImg.setHeight(16);
					editImg.setWidth(16);
					editImg.addClickHandler(event -> onEdit());

					rollOverCanvas.addMember(editImg);
				}
				return rollOverCanvas;

			}
		};
		list.setShowRollOverCanvas(true);
		list.setShowRollUnderCanvas(false);
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowAllRecords(true);
		list.setCanEdit(false);
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

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		list.addDoubleClickHandler(event -> onEdit());

		// Initialize with english language
		langSelector.setValue("en");

		setMembers(hint, toolStrip, list);

		reload();
	}

	private void onEdit() {
		new MessageTemplateEditor(list, rollOverRecord).show();
	}

	/**
	 * Sends the patterns in the grid to the server to save them.
	 */
	private void saveTemplates() {
		String lang = langSelector.getValueAsString();
		if (lang == null || lang.isEmpty())
			return;

		Record[] records = list.getRecords();
		List<GUIMessageTemplate> templates = new ArrayList<>();
		for (Record rec : records) {
			GUIMessageTemplate template = new GUIMessageTemplate();
			template.setId(Long.parseLong(rec.getAttributeAsString("id")));
			template.setLanguage(lang);
			template.setName(rec.getAttributeAsString("name"));
			template.setSubject(rec.getAttributeAsString(SUBJECT));
			template.setBody(rec.getAttributeAsString("body"));
			template.setType(rec.getAttributeAsString("type"));
			templates.add(template);
		}

		MessageService.Instance.get().saveTemplates(templates, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(Void arg0) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem copyFromDefault = new MenuItem();
		copyFromDefault.setTitle(I18N.message("copyfromdefault"));
		copyFromDefault.addClickHandler((MenuItemClickEvent event) -> {
			ListGridRecord[] records = list.getSelectedRecords();
			List<Long> ids = new ArrayList<>();
			for (int i = 0; i < records.length; i++) {
				// Avoid deletion of default templates
				if (!"en".equals(records[i].getAttributeAsString(LANGUAGE)))
					ids.add(records[i].getAttributeAsLong("id"));
			}

			MessageService.Instance.get().deleteTemplates(ids, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(Void arg0) {
					reload();
				}
			});
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> MessageService.Instance.get()
				.deleteTemplates(list.getSelectedRecord().getAttributeAsString("name"), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void arg) {
						reload();
					}
				}));

		delete.setEnabled(!"system".equals(list.getSelectedRecord().getAttributeAsString("type")));
		copyFromDefault.setEnabled(!"en".equals(langSelector.getValueAsString()));

		contextMenu.setItems(copyFromDefault, new MenuItemSeparator(), delete);
		contextMenu.showContextMenu();
	}

	private void reload() {
		String lang = langSelector.getValueAsString();

		MessageService.Instance.get().loadTemplates(lang, null, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(List<GUIMessageTemplate> templates) {
				List<ListGridRecord> records = new ArrayList<>();
				for (GUIMessageTemplate pat : templates) {
					ListGridRecord rec = new ListGridRecord();
					rec.setAttribute("id", pat.getId());
					rec.setAttribute("name", pat.getName());
					rec.setAttribute(LANGUAGE, pat.getName());
					rec.setAttribute(SUBJECT, pat.getSubject());
					rec.setAttribute("body", pat.getBody());
					rec.setAttribute("type", pat.getType());
					records.add(rec);
				}
				list.setData(records.toArray(new ListGridRecord[0]));
			}
		});
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}