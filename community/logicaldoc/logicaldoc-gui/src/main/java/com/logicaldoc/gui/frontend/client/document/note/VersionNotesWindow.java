package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This window shows the notes of a version
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class VersionNotesWindow extends Window {

	public VersionNotesWindow(GUIDocument doc, String fileVer) {
		super();

		String fileVersion = fileVer;
		if (fileVersion == null)
			fileVersion = doc.getFileVersion();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("notes") + " - " + doc.getFileName() + " v" + fileVersion);
		setWidth100();
		setHeight100();

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		ListGridField id = new ListGridField("id", I18N.message("id"), 50);
		id.setHidden(true);

		ListGridField userId = new ListGridField("userId", "userid", 50);
		userId.setHidden(true);

		ListGridField user = new ListGridField("user", I18N.message("author"), 200);
		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.LEFT);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);
		ListGridField page = new ListGridField("page", I18N.message("page"), 80);
		page.setWidth("*");

		ListGrid notesGrid = new ListGrid() {
			@Override
			protected Canvas getExpansionComponent(final ListGridRecord record) {
				return new HTMLFlow("<div class='details'>"
						+ (record.getAttributeAsString("message") != null ? record.getAttributeAsString("message") : "")
						+ "</div>");
			}
		};
		notesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		notesGrid.setCanFreezeFields(true);
		notesGrid.setAutoFetchData(true);
		notesGrid.setDataSource(new NotesDS(null, doc.getId(), fileVersion, null));
		notesGrid.setFields(id, userId, user, date, page);
		notesGrid.setWidth100();
		notesGrid.setCanExpandRecords(true);
		notesGrid.setExpansionMode(ExpansionMode.DETAIL_FIELD);
		notesGrid.setDetailField("message");

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		ToolStripButton annotations = new ToolStripButton();
		annotations.setTitle(I18N.message("annotations"));
		annotations.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AnnotationsWindow annotationWnd = new AnnotationsWindow(doc,
						fileVer != null ? fileVer : doc.getFileVersion(), null, false);
				annotationWnd.show();
			}
		});

		toolStrip.addButton(annotations);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(toolStrip);
		addItem(notesGrid);
	}
}