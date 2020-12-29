package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentHistoryDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the history of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class HistoryPanel extends DocumentDetailTab {

	public HistoryPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField user = new ListGridField("user", I18N.message("user"), 100);
		ListGridField event = new ListGridField("event", I18N.message("event"), 200);
		ListGridField version = new ListGridField("version", I18N.message("version"), 70);
		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.CENTER);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);
		ListGridField comment = new ListGridField("comment", I18N.message("comment"));
		ListGridField fileName = new ListGridField("filename", I18N.message("filename"));
		ListGridField path = new ListGridField("path", I18N.message("path"));
		ListGridField sid = new ListGridField("sid", I18N.message("sid"));
		ListGridField ip = new ListGridField("ip", I18N.message("ip"));

		final RefreshableListGrid list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new DocumentHistoryDS(document.getId(), null));
		if (Menu.enabled(Menu.SESSIONS))
			list.setFields(user, event, date, comment, version, fileName, path, sid, ip);
		else
			list.setFields(user, event, date, comment, version, fileName, path);

		list.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				LD.askForValue(I18N.message("comment"), I18N.message("comment"),
						list.getSelectedRecord().getAttributeAsString("comment"), new ValueCallback() {
							@Override
							public void execute(final String value) {
							}
						});
				event.cancel();
			}
		});

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();

		SpinnerItem maxItem = ItemFactory.newSpinnerItem("max", "display",
				Session.get().getConfigAsInt("gui.maxhistories"), 1, (Integer) null);
		maxItem.setWidth(70);
		maxItem.setStep(20);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.setHint(I18N.message("elements"));
		buttons.addFormItem(maxItem);
		maxItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				list.refresh(new DocumentHistoryDS(document.getId(), Integer.parseInt(maxItem.getValueAsString())));
			}
		});

		buttons.addSeparator();
		
		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		buttons.addButton(export);
		export.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				Util.exportCSV(list, true);
			}
		});

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		buttons.addButton(print);
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Canvas.printComponents(new Object[] { list });
			}
		});
		
		VLayout container = new VLayout();
		container.setMembersMargin(3);
		container.addMember(list);
		container.addMember(buttons);
		addMember(container);
	}
}