package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager.DocumentProtectionHandler;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.events.ShowContextMenuEvent;
import com.smartgwt.client.widgets.events.ShowContextMenuHandler;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.tile.TileGrid;
import com.smartgwt.client.widgets.tile.events.DataArrivedEvent;
import com.smartgwt.client.widgets.tile.events.SelectionChangedEvent;
import com.smartgwt.client.widgets.viewer.DetailFormatter;
import com.smartgwt.client.widgets.viewer.DetailViewerField;

/**
 * Grid used to show a documents gallery during navigation or searches.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DocumentsTileGrid extends TileGrid implements DocumentsGrid, DocumentObserver {
	private Cursor cursor;

	private GUIFolder folder = null;

	private List<DetailViewerField> fields = new ArrayList<DetailViewerField>();

	public DocumentsTileGrid(GUIFolder folder) {
		this.folder = folder;
		setTileWidth(200);
		setTileHeight(250);
		setAutoFetchData(true);
		setSelectionType(SelectionStyle.MULTIPLE);
		setShowAllRecords(false);
		setCanReorderTiles(false);
		setCanDrag(folder != null && folder.isMove());
		setWidth100();

		prepareDataSource(folder);
		
		addThumbnail();

		addFilename();

		setFields(fields.toArray(new DetailViewerField[0]));

		addDataArrivedHandler(new com.smartgwt.client.widgets.tile.events.DataArrivedHandler() {

			@Override
			public void onDataArrived(DataArrivedEvent event) {
				if (cursor != null) {
					cursor.setMessage(I18N.message("showndocuments", Integer.toString(getCount())));
				}

				sortByProperty("filename", true);
			}
		});

		DocumentController.get().addObserver(this);
	}

	private void addFilename() {
		DetailViewerField filename = new DetailViewerField("filename");
		filename.setDetailFormatter(new DetailFormatter() {

			@Override
			public String format(Object value, Record rec, DetailViewerField field) {
				try {
					String html = "<table style='margin-top:2px' align='center' border='0' cellspacing='0'>";

					// The title row
					html += "<tr><td>" + Util.imageHTML(rec.getAttribute("icon") + ".png") + "</td><td>" + value
							+ "</td></tr></table>";
					html += "<table align='center' border='0' cellspacing='0'><tr>";

					// The status row
					if (rec.getAttributeAsBoolean("bookmarked"))
						html += "<td>" + DocUtil.getBookmarkedIcon(rec.getAttributeAsBoolean("bookmarked"))
								+ "</td>";
					html += "<td>" + AwesomeFactory.getIndexedIcon(rec.getAttributeAsInt("indexed")) + "</td>";

					// The locked icon
					if (rec.getAttribute("status") != null) {
						Integer status = rec.getAttributeAsInt("status");
						if (status != null && status.intValue() > 0) {
							String alt = "";
							if (status == Constants.DOC_CHECKED_OUT || status == Constants.DOC_LOCKED)
								alt = I18N.message("lockedby") + " " + rec.getAttributeAsString("lockUser");
							html += "<td><span title='" + alt + "'>" + DocUtil.getLockedIcon(status) + "</span></td>";
						}
					}

					html += "<td>" + DocUtil.getPasswordProtectedIcon(rec.getAttributeAsBoolean("password"))
							+ "</td>";
					html += "<td>" + DocUtil.getImmutableIcon(rec.getAttributeAsInt("immutable")) + "</td>";
					html += "<td>" + DocUtil.getSignedIcon(rec.getAttributeAsInt("signed")) + "</td>";
					html += "<td>" + DocUtil.getStampedIcon(rec.getAttributeAsInt("stamped")) + "</td>";
					html += "</tr></table>";

					return html;
				} catch (Throwable e) {
					return "";
				}
			}
		});
		fields.add(filename);
	}

	private void addThumbnail() {
		DetailViewerField thumbnail = new DetailViewerField("thumbnail");
		thumbnail.setDetailFormatter(new DetailFormatter() {

			@Override
			public String format(Object value, Record rec, DetailViewerField field) {
				int thumbnailSize = 200;
				if (Session.get().getConfig("gui.thumbnail.size") != null)
					thumbnailSize = Integer.parseInt(Session.get().getConfig("gui.thumbnail.size"));

				try {
					if ("folder".equals(rec.getAttribute("type")))
						return Util.imageHTML("folder_tile.png", null, thumbnailSize, null);
					else {
						long docId = Long.parseLong(rec.getAttribute("id"));
						if (!rec.getAttributeAsBoolean("password") || DocumentProtectionManager.isUnprotected(docId))
							return Util.thumbnailImgageHTML(docId, null, null, thumbnailSize);
						else
							return Util.imageHTML("blank.png", null,
									"width:" + thumbnailSize + "px height:" + thumbnailSize + "px");
					}
				} catch (Throwable e) {
					return "";
				}
			}
		});
		fields.add(thumbnail);
	}

	private void prepareDataSource(GUIFolder folder) {
		DocumentsDS ds = null;
		if (folder != null) {
			int max = loadGridLayout(folder);
			ds = new DocumentsDS(folder, null, max, 1, null, false, false, null);
		}
		
		if (ds == null) {
			/*
			 * We are searching
			 */
			setSelectionType(SelectionStyle.SINGLE);
		} else {
			setDataSource(ds);
		}
	}

	@Override
	public void updateDocument(GUIDocument document) {
		Record rec = findRecord(document.getId());
		if (rec != null) {
			DocumentGridUtil.updateRecord(document, rec);
			Canvas tile = getTile(rec);
			tile.redraw();
		}
	}

	@Override
	public void setDocuments(GUIDocument[] documents) {
		Record[] records = new Record[0];
		if (documents != null && documents.length > 0) {
			records = new Record[documents.length];
			for (int i = 0; i < documents.length; i++) {
				GUIDocument doc = documents[i];
				Record rec = DocumentGridUtil.fromDocument(doc);
				records[i] = rec;
			}
		}
		setData(records);
	}

	@Override
	public GUIDocument getSelectedDocument() {
		return DocumentGridUtil.toDocument(getSelectedRecord());
	}

	@Override
	public GUIDocument[] getSelectedDocuments() {
		return DocumentGridUtil.toDocuments(getSelection());
	}

	@Override
	public GUIDocument[] getDocuments() {
		return DocumentGridUtil.toDocuments(getRecordList().toArray());
	}

	@Override
	public int getSelectedIndex() {
		return super.getRecordIndex(getSelectedRecord());
	}

	@Override
	public long[] getSelectedIds() {
		return DocumentGridUtil.getIds(getSelection());
	}

	@Override
	public Long[] getSelectedIdsAsLong() {
		return DocumentGridUtil.getIdsAsLong(getSelection());
	}

	@Override
	public long[] getIds() {
		return DocumentGridUtil.getIds(getRecordList().toArray());
	}

	@Override
	public void deselectAll() {
		deselectAllRecords();
	}

	@Override
	public void setCanExpandRows() {
		// Nothing to do
	}

	@Override
	public int getCount() {
		RecordList rl = getRecordList();
		if (rl != null)
			return getRecordList().getLength();
		else
			return 0;
	}

	@Override
	public int getSelectedCount() {
		Record[] selection = getSelection();
		if (selection != null)
			return selection.length;
		else
			return 0;
	}

	@Override
	public void showFilters(boolean showFilters) {
		// Nothing to do
	}

	@Override
	public void selectDocument(long docId) {
		deselectAll();
		RecordList rlist = getDataAsRecordList();
		Record rec = rlist.find("id", Long.toString(docId));
		if (rec != null)
			selectRecord(rec);
	}

	@Override
	public void expandVisibleRows() {
		// Nothing to do
	}

	@Override
	public void setCanDrag(boolean drag) {
		super.setCanDrag(drag);
		setCanDragTilesOut(drag);
	}

	@Override
	public void registerDoubleClickHandler(final DoubleClickHandler handler) {
		addDoubleClickHandler(new DoubleClickHandler() {

			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				GUIDocument selectedDocument = getSelectedDocument();
				if (selectedDocument == null)
					return;
				DocumentProtectionManager.askForPassword(selectedDocument.getId(), new DocumentProtectionHandler() {
					@Override
					public void onUnprotected(GUIDocument document) {
						handler.onDoubleClick(null);
					}
				});
			}
		});
	}

	@Override
	public void registerSelectionChangedHandler(final SelectionChangedHandler handler) {
		addSelectionChangedHandler(new com.smartgwt.client.widgets.tile.events.SelectionChangedHandler() {

			@Override
			public void onSelectionChanged(SelectionChangedEvent event) {
				GUIDocument selectedDocument = getSelectedDocument();
				if (selectedDocument == null)
					return;
				DocumentProtectionManager.askForPassword(selectedDocument.getId(), new DocumentProtectionHandler() {
					@Override
					public void onUnprotected(GUIDocument document) {
						handler.onSelectionChanged(null);
					}
				});
			}
		});
	}

	@Override
	public void registerCellContextClickHandler(final CellContextClickHandler handler) {
		addShowContextMenuHandler(new ShowContextMenuHandler() {

			@Override
			public void onShowContextMenu(final ShowContextMenuEvent event) {
				GUIDocument selectedDocument = getSelectedDocument();
				if (selectedDocument == null)
					return;
				DocumentProtectionManager.askForPassword(selectedDocument.getId(), new DocumentProtectionHandler() {
					@Override
					public void onUnprotected(GUIDocument document) {
						handler.onCellContextClick(null);
					}
				});
				if (event != null)
					event.cancel();
			}
		});
	}

	@Override
	public void registerDataArrivedHandler(final DataArrivedHandler handler) {
		addDataArrivedHandler(new com.smartgwt.client.widgets.tile.events.DataArrivedHandler() {

			@Override
			public void onDataArrived(DataArrivedEvent event) {
				handler.onDataArrived(null);
			}
		});
	}

	@Override
	public void removeSelectedDocuments() {
		removeSelectedData();
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentModified(GUIDocument document) {
		updateDocument(document);
	}

	@Override
	public void onDocumentStored(GUIDocument document) {
		if (folder != null && document.getFolder().getId() == folder.getId()) {
			Record doc = findRecord(document.getId());
			if (doc != null)
				return;

			addData(DocumentGridUtil.fromDocument(document));
			cursor.setMessage(I18N.message("showndocuments", Integer.toString(getData().length)));
		}
	}

	@Override
	public void onDocumentsDeleted(GUIDocument[] documents) {
		for (GUIDocument doc : documents) {
			Record rec = findRecord(doc.getId());
			if (rec != null) {
				try {
					removeData(rec);
					cursor.setMessage(I18N.message("showndocuments",
							"" + (getData() != null ? Integer.toString(getData().length) : 0)));
				} catch (Throwable t) {
					// Nothing to do
				}
			}
		}
	}

	@Override
	public void onDocumentCheckedIn(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentCheckedOut(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentLocked(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentUnlocked(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		if (folder != null && document.getFolder().getId() != folder.getId())
			onDocumentsDeleted(new GUIDocument[] { document });
	}

	@Override
	public void onDocumentBeginEditing(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		// Nothing to do
	}

	private Record findRecord(long docId) {
		Record rec = find(new AdvancedCriteria("id", OperatorId.EQUALS, docId));
		if (rec == null)
			rec = find(new AdvancedCriteria("docref", OperatorId.EQUALS, docId));
		return rec;
	}

	@Override
	public GUIFolder getFolder() {
		return folder;
	}

	@Override
	public void destroy() {
		DocumentController.get().removeObserver(this);
	}

	@Override
	protected void onUnload() {
		destroy();
		super.onUnload();
	}

	@Override
	protected void onDestroy() {
		destroy();
		super.onDestroy();
	}

	@Override
	public void fetchNewData(DocumentsDS ds) {
		if (ds.getFolder() != null) {
			this.folder = ds.getFolder();
			setCanDrag(folder.isMove());
		}
		getDataSource().destroy();
		setDataSource(ds, fields.toArray(new DetailViewerField[0]));
		fetchData();
		redraw();
	}

	@Override
	public void setGridCursor(Cursor gridCursor) {
		this.cursor = gridCursor;
	}

	@Override
	public Cursor getGridCursor() {
		return cursor;
	}

	@Override
	public int loadGridLayout(GUIFolder folder) {
		this.folder = folder;
		String previouslySavedState = folder != null ? folder.getGrid() : null;
		if (previouslySavedState == null || previouslySavedState.isEmpty())
			previouslySavedState = Session.get().getUser().getDocsGrid();

		Integer pageSize = DocumentGridUtil.getPageSizeFromSpec(previouslySavedState);
		if (pageSize == null)
			pageSize = Session.get().getConfigAsInt("gui.document.pagesize");

		if (getGridCursor() != null) {
			getGridCursor().setPageSize(pageSize);
			if (folder != null)
				getGridCursor().setTotalRecords(folder.getDocumentCount());
		}

		return pageSize;
	}
}