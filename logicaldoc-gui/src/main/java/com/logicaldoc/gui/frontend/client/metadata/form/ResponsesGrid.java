package com.logicaldoc.gui.frontend.client.metadata.form;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.preview.PreviewPopup;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Grid used to show athe responses of a given form.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class ResponsesGrid extends RefreshableListGrid {

	private static final String LANGUAGE = "language";

	protected GUIForm form = null;

	public ResponsesGrid(GUIForm form) {
		this.form = form;
		setEmptyMessage(I18N.message("notitemstoshow"));
		setCanFreezeFields(true);
		setAutoFetchData(true);
		setFilterOnKeypress(true);
		setShowRecordComponents(true);
		setShowRecordComponentsByCell(true);
		setSaveLocally(true);

		prepareFields();

		addCellContextClickHandler((CellContextClickEvent event) -> {
			event.cancel();
			showContextMenu();
		});

		ResponsesDS dataSource = new ResponsesDS(form, 100);
		setDataSource(dataSource);
	}

	/**
	 * Prepares the map that contains all the possible fields we can use
	 */
	private void prepareFields() {
		FileNameListGridField filename = new FileNameListGridField();
		filename.setHidden(true);
		filename.setCanFilter(true);

		ListGridField id = new ListGridField("id", I18N.getAttributeLabel("id"), 60);
		id.setHidden(true);

		ListGridField size = new FileSizeListGridField("size", I18N.getAttributeLabel("size"));
		size.setHidden(true);

		ListGridField created = new DateListGridField("created", "created");

		final LinkedHashMap<String, String> languages = I18N.getSupportedLanguages(false);

		ListGridField language = new ListGridField(LANGUAGE, I18N.message(LANGUAGE), 100);
		language.setType(ListGridFieldType.TEXT);
		language.setCanFilter(false);
		language.setAlign(Alignment.CENTER);
		language.setHidden(true);
		language.setCellFormatter((value, rec, rowNum, colNum) -> languages.get(rec.getAttribute(LANGUAGE)));

		ListGridField respondent = new ListGridField("_respondent", I18N.message("respondent"), 200);
		respondent.setWidth(200);
		respondent.setCanSort(false);

		ListGridField folder = new ListGridField("folder", I18N.message("folder"), 200);
		folder.setWidth(200);
		folder.setHidden(true);
		folder.setCanSort(false);

		List<ListGridField> fields = new ArrayList<>();
		fields.add(id);
		fields.add(filename);
		fields.add(size);
		fields.add(created);
		fields.add(respondent);
		fields.add(folder);

		for (String name : form.getAttributeNames()) {
			if (name != null && !"".equals(name)) {
				ListGridField ext = new ListGridField("ext_" + name, Session.get().getInfo().getAttributeLabel(name),
						100);
				GUIAttribute attDef = Session.get().getInfo().getAttributeDefinition(name);
				if (attDef != null) {
					if (attDef.getType() == GUIAttribute.TYPE_DATE) {
						ext = new DateListGridField("ext_" + name, Session.get().getInfo().getAttributeLabel(name));
						ext.setTitle(Session.get().getInfo().getAttributeLabel(name));
					} else if (attDef.getType() == GUIAttribute.TYPE_INT) {
						ext.setAlign(Alignment.RIGHT);
						ext.setType(ListGridFieldType.INTEGER);
						ext.setCanFilter(false);
					} else if (attDef.getType() == GUIAttribute.TYPE_DOUBLE) {
						ext.setAlign(Alignment.RIGHT);
						ext.setType(ListGridFieldType.FLOAT);
						ext.setCanFilter(false);
					} else if (attDef.getType() == GUIAttribute.TYPE_USER) {
						ext = new UserListGridField("ext_" + name, "ext_" + name,
								Session.get().getInfo().getAttributeLabel(name));
						ext.setTitle(Session.get().getInfo().getAttributeLabel(name));
					}
				}

				ext.setCanFilter(true);
				ext.setCanSort(true);
				fields.add(ext);
			}
		}

		setFields(fields.toArray(new ListGridField[0]));
	}

	private void showContextMenu() {
		final ListGridRecord[] selection = getSelectedRecords();

		Menu contextMenu = new Menu();

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler((MenuItemClickEvent event) -> {
			ListGridRecord rec = getSelectedRecord();
			if (rec == null)
				return;

			DocumentsPanel.get().openInFolder(rec.getAttributeAsLong("id"));
		});

		MenuItem preview = new MenuItem();
		preview.setTitle(I18N.message("preview"));
		preview.addClickHandler((MenuItemClickEvent event) -> {
			PreviewPopup iv = null;

			if (selection.length == 1) {
				GUIDocument doc = DocumentGridUtil.toDocument(getSelectedRecord());
				if (doc.getDocRef() != null) {
					/*
					 * in case of alias the data servlet inverts the docId and
					 * the docRef so in order to have the preview to do the
					 * right security checks we have to restore the correct ids
					 */
					long aliasId = doc.getDocRef();
					doc.setDocRef(doc.getId());
					doc.setId(aliasId);
				}
				iv = new PreviewPopup(doc);
			} else {
				GUIDocument[] docs = DocumentGridUtil.toDocuments(selection);
				for (GUIDocument doc : docs) {
					/*
					 * in case of alias the data servlet inverts the docId and
					 * the docRef so in order to have the preview to do the
					 * right security checks we have to restore the correct ids
					 */
					long aliasId = doc.getDocRef();
					doc.setDocRef(doc.getId());
					doc.setId(aliasId);
				}
				iv = new PreviewPopup(docs, 0);
			}
			iv.show();
		});

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler((MenuItemClickEvent event) -> DocumentService.Instance.get()
				.delete(DocumentGridUtil.getIds(selection), new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						refresh(getDataSource());
					}
				}));

		contextMenu.setItems(preview, openInFolder, delete);
		contextMenu.showContextMenu();
	}

	@Override
	public DateDisplayFormat getDateFormatter() {
		return I18N.getDateDisplayFormat(false);
	}

	@Override
	public DateDisplayFormat getDatetimeFormatter() {
		return I18N.getDateDisplayFormat(true);
	}
}