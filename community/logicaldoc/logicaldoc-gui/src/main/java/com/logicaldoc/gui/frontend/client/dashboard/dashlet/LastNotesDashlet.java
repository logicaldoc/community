package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.HeaderControl.HeaderIcon;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.menu.Menu;

/**
 * Portlet specialized in listing the most recent comments of the current user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class LastNotesDashlet extends Dashlet {

	private NotesDS dataSource;

	protected RefreshableListGrid list;

	public LastNotesDashlet() {
		super(Constants.DASHLET_LAST_NOTES);

		setTitle(AwesomeFactory.getIconHtml("sticky-note", I18N.message("lastnotes")));

		if (Feature.enabled(Feature.NOTES)) {
			init();
		} else {
			addItem(new FeatureDisabled());
		}
	}

	private void init() {
		ListGridField date = new ListGridField("date", I18N.message("date"), 90);
		date.setAlign(Alignment.CENTER);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(true));
		date.setCanFilter(false);
		ListGridField title = new ListGridField("title", I18N.message("note"));
		ListGridField docFilename = new ListGridField("docFilename", I18N.message("filename"));
		docFilename.setAutoFitWidth(true);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setShowHeader(false);
		list.setCanSelectAll(false);
		list.setSelectionType(SelectionStyle.NONE);
		list.setHeight100();
		list.setBorder("0px");
		list.setDataSource(getDataSource());
		list.setFields(date, docFilename, title);

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				if (event != null)
					event.cancel();
				Record record = event.getRecord();
				DocumentService.Instance.get().getById(Long.parseLong(record.getAttributeAsString("docId")),
						new AsyncCallback<GUIDocument>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUIDocument document) {
								Menu contextMenu = prepareContextMenu(document);
								contextMenu.showContextMenu();
							}
						});
			}
		});

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				Record record = event.getRecord();
				DocumentsPanel.get().openInFolder(Long.parseLong(record.getAttributeAsString("docId")));
			}
		});

		HeaderControl refresh = new HeaderControl(HeaderControl.REFRESH, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				list.refresh(getDataSource());
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, refresh, HeaderControls.MAXIMIZE_BUTTON,
				HeaderControls.CLOSE_BUTTON);

		addItem(list);
	}

	private NotesDS getDataSource() {
		long userId = Session.get().getUser().getId();
		return new NotesDS(userId, null, null);
	}

	@Override
	public void destroy() {
		super.destroy();
		if (dataSource != null)
			dataSource.destroy();
	}
}