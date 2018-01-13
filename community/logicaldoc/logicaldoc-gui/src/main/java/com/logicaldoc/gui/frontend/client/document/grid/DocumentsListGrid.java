package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.formatters.FileSizeCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager.DocumentProtectionHandler;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.document.RatingDialog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellClickEvent;
import com.smartgwt.client.widgets.grid.events.CellClickHandler;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Grid used to show a documents list in different contexts.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class DocumentsListGrid extends ListGrid implements DocumentsGrid, DocumentObserver {

	protected Cursor cursor = null;

	protected GUIFolder folder = null;

	protected int totalRecords = 0;

	// Stores all the possible fields we can use in a grid of documents
	protected Map<String, ListGridField> fieldsMap = new HashMap<String, ListGridField>();

	/**
	 * Prepares the map that contains all the possible fields we can use
	 */
	private void prepareFieldsMap() {
		ListGridField id = new ListGridField("id", I18N.getAttributeLabel("id"), 50);
		id.setHidden(true);
		fieldsMap.put(id.getName(), id);

		ListGridField size = new ListGridField("size", I18N.getAttributeLabel("size"), 70);
		size.setAlign(Alignment.RIGHT);
		size.setType(ListGridFieldType.FLOAT);
		size.setCellFormatter(new FileSizeCellFormatter());
		size.setCanFilter(false);
		fieldsMap.put(size.getName(), size);

		ListGridField icon = new ListGridField("icon", " ", 21);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		icon.setCanFilter(false);
		fieldsMap.put(icon.getName(), icon);

		ListGridField version = new ListGridField("version", I18N.getAttributeLabel("version"), 55);
		version.setAlign(Alignment.CENTER);
		version.setHidden(true);
		version.setCanFilter(true);
		fieldsMap.put(version.getName(), version);

		ListGridField lastModified = new ListGridField("lastModified", I18N.getAttributeLabel("lastModified"), 110);
		lastModified.setAlign(Alignment.CENTER);
		lastModified.setType(ListGridFieldType.DATE);
		lastModified.setCellFormatter(new DateCellFormatter(false));
		lastModified.setCanFilter(false);
		lastModified.setHidden(true);
		fieldsMap.put(lastModified.getName(), lastModified);

		ListGridField publisher = new ListGridField("publisher", I18N.message("publisher"), 90);
		publisher.setAlign(Alignment.CENTER);
		publisher.setCanFilter(true);
		publisher.setCanSort(false);
		fieldsMap.put(publisher.getName(), publisher);

		ListGridField published = new ListGridField("published", I18N.message("published"), 110);
		published.setAlign(Alignment.CENTER);
		published.setType(ListGridFieldType.DATE);
		published.setCellFormatter(new DateCellFormatter(false));
		published.setCanFilter(false);
		fieldsMap.put(published.getName(), published);

		ListGridField creator = new ListGridField("creator", I18N.message("creator"), 90);
		creator.setAlign(Alignment.CENTER);
		creator.setCanFilter(true);
		creator.setHidden(true);
		creator.setCanSort(false);
		fieldsMap.put(creator.getName(), creator);

		ListGridField created = new ListGridField("created", I18N.message("created"), 110);
		created.setAlign(Alignment.CENTER);
		created.setType(ListGridFieldType.DATE);
		created.setCellFormatter(new DateCellFormatter(false));
		created.setCanFilter(false);
		created.setHidden(true);
		fieldsMap.put(created.getName(), created);

		ListGridField customId = new ListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		fieldsMap.put(customId.getName(), customId);

		ListGridField type = new ListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		fieldsMap.put(type.getName(), type);

		ListGridField statusIcons = new ListGridField("statusIcons", " ");
		statusIcons.setWidth(90);
		statusIcons.setCanFilter(false);
		statusIcons.setCanSort(false);
		fieldsMap.put(statusIcons.getName(), statusIcons);

		ListGridField indexed = new ListGridField("indexed", " ", 20);
		indexed.setType(ListGridFieldType.IMAGE);
		indexed.setCanSort(false);
		indexed.setAlign(Alignment.CENTER);
		indexed.setShowDefaultContextMenu(false);
		indexed.setImageURLPrefix(Util.imagePrefix());
		indexed.setImageURLSuffix(".png");
		indexed.setCanFilter(false);
		fieldsMap.put(indexed.getName(), indexed);

		ListGridField filename = new ListGridField("filename", I18N.message("filename"), 200);
		filename.setHidden(true);
		filename.setCanFilter(true);
		fieldsMap.put(filename.getName(), filename);

		ListGridField lockUserId = new ListGridField("lockUserId", " ", 24);
		lockUserId.setHidden(true);
		lockUserId.setCanFilter(false);
		lockUserId.setCanSort(false);
		fieldsMap.put(lockUserId.getName(), lockUserId);

		ListGridField rating = new ListGridField("rating", I18N.message("rating"), 95);
		rating.setType(ListGridFieldType.IMAGE);
		rating.setCanSort(false);
		rating.setCanFilter(false);
		rating.setAlign(Alignment.CENTER);
		rating.setImageURLPrefix(Util.imagePrefix());
		rating.setImageURLSuffix(".png");
		rating.setImageWidth(88);
		rating.setHidden(true);
		fieldsMap.put(rating.getName(), rating);

		ListGridField fileVersion = new ListGridField("fileVersion", I18N.message("fileversion"), 70);
		fileVersion.setHidden(false);
		fileVersion.setCanFilter(true);
		fileVersion.setAlign(Alignment.CENTER);
		fieldsMap.put(fileVersion.getName(), fileVersion);

		ListGridField comment = new ListGridField("comment", I18N.message("comment"), 300);
		comment.setHidden(true);
		comment.setCanFilter(true);
		comment.setCanSort(true);
		fieldsMap.put(comment.getName(), comment);

		ListGridField wfStatus = new ListGridField("workflowStatus", I18N.message("workflowstatus"), 100);
		wfStatus.setHidden(true);
		wfStatus.setCanFilter(true);
		wfStatus.setCanSort(true);
		wfStatus.setAlign(Alignment.LEFT);
		fieldsMap.put(wfStatus.getName(), wfStatus);

		ListGridField startPublishing = new ListGridField("startPublishing", I18N.message("startpublishing"), 110);
		startPublishing.setAlign(Alignment.CENTER);
		startPublishing.setType(ListGridFieldType.DATE);
		startPublishing.setCellFormatter(new DateCellFormatter(false));
		startPublishing.setCanFilter(false);
		startPublishing.setCanSort(true);
		startPublishing.setHidden(true);
		fieldsMap.put(startPublishing.getName(), startPublishing);

		ListGridField stopPublishing = new ListGridField("stopPublishing", I18N.message("stoppublishing"), 110);
		stopPublishing.setAlign(Alignment.CENTER);
		stopPublishing.setType(ListGridFieldType.DATE);
		stopPublishing.setCellFormatter(new DateCellFormatter(false));
		stopPublishing.setCanFilter(false);
		stopPublishing.setCanSort(true);
		stopPublishing.setHidden(true);
		fieldsMap.put(stopPublishing.getName(), stopPublishing);

		ListGridField publishedStatus = new ListGridField("publishedStatus", I18N.message("published"), 50);
		publishedStatus.setHidden(true);
		publishedStatus.setCanFilter(true);
		publishedStatus.setCanSort(true);
		fieldsMap.put(publishedStatus.getName(), publishedStatus);

		ListGridField template = new ListGridField("template", I18N.message("template"), 150);
		template.setAlign(Alignment.LEFT);
		template.setHidden(true);
		template.setCanFilter(true);
		template.setCanSort(false);
		fieldsMap.put(template.getName(), template);

		ListGridField thumbnail = new ListGridField("thumbnail", I18N.message("thumbnail"), 200);
		thumbnail.setHidden(true);
		thumbnail.setCanFilter(false);
		thumbnail.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				try {
					return Util.thumbnailImgageHTML(Long.parseLong(record.getAttribute("id")), null, 200, null);
				} catch (Throwable e) {
					return "";
				}
			}
		});
		fieldsMap.put(thumbnail.getName(), thumbnail);

		// For search only
		ListGridField folder = new ListGridField("folder", I18N.message("folder"), 200);
		folder.setWidth(200);
		folder.setHidden(true);
		folder.setCanSort(false);
		fieldsMap.put(folder.getName(), folder);

		// For search only
		ListGridField score = new ListGridField("score", I18N.message("score"), 120);
		score.setCanFilter(false);
		score.setHidden(true);
		score.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				try {
					int score = record.getAttributeAsInt("score");
					int red = 100 - score > 0 ? 100 - score : 0;
					return "<img src='" + Util.imageUrl("dotblue.gif") + "' style='width: " + score
							+ "px; height: 8px' title='" + score + "%'/>" + "<img src='" + Util.imageUrl("dotgrey.gif")
							+ "' style='width: " + red + "px; height: 8px' title='" + score + "%'/>";
				} catch (Throwable e) {
					return "";
				}
			}
		});
		fieldsMap.put(score.getName(), score);

		String[] extNames = Session.get().getInfo().getConfig("search.extattr").split(",");
		for (String name : extNames) {
			if (name != null && !"".equals(name)) {
				ListGridField ext = new ListGridField("ext_" + name, Session.get().getInfo().getAttributeLabel(name),
						100);
				ext.setHidden(true);
				ext.setCanFilter(true);
				fieldsMap.put(ext.getName(), ext);
			}
		}
	}

	public DocumentsListGrid() {
		this(null, 0);
	}

	public DocumentsListGrid(GUIFolder folder, int totalRecords) {
		this.totalRecords = totalRecords;
		this.folder = folder;
		setEmptyMessage(I18N.message("notitemstoshow"));
		setCanFreezeFields(true);
		setAutoFetchData(true);
		setFilterOnKeypress(true);
		setShowRecordComponents(true);
		setShowRecordComponentsByCell(true);
		setSaveLocally(true);

		prepareFieldsMap();

		addCellClickHandler(new CellClickHandler() {
			@Override
			public void onCellClick(CellClickEvent event) {
				if ("rating".equals(getFieldName(event.getColNum()))) {
					long id = Long.parseLong(getSelectedRecord().getAttribute("id"));
					String ratingImageName = getSelectedRecord().getAttribute("rating");
					final int docRating = Integer.parseInt(ratingImageName.replace("rating", ""));
					DocumentService.Instance.get().getRating(id, new AsyncCallback<GUIRating>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIRating rating) {
							if (rating != null) {
								RatingDialog dialog = new RatingDialog(docRating, rating);
								dialog.show();
							}
						}
					});
					event.cancel();
				}
			}
		});

		addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				if (cursor != null) {
					cursor.setMessage(I18N.message("showndocuments", Integer.toString(getCount())));
					cursor.setTotalRecords(getTotalRecords());
				}
			}
		});

		DocumentController.get().addObserver(this);
	}

	@Override
	public void deselectAll() {
		deselectAllRecords();
	}

	@Override
	protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
		if (getFieldName(colNum).equals("filename")) {
			int immutable = 0;
			if (record.getAttribute("immutable") != null)
				immutable = record.getAttributeAsInt("immutable");

			if (immutable == 1 || !"yes".equals(record.getAttribute("publishedStatus"))) {
				return "color: #888888; font-style: italic;";
			} else {
				return super.getCellCSSText(record, rowNum, colNum);
			}
		} else {
			return super.getCellCSSText(record, rowNum, colNum);
		}
	}

	@Override
	public void updateDocument(GUIDocument document) {
		Record record = findRecord(document.getId());
		if (record != null) {
			GridUtil.updateRecord(document, record);
			refreshRow(record);
		}
	}

	private void refreshRow(Record record) {
		invalidateRecordComponents();
		refreshRecordComponent(getRecordIndex(record));
		refreshFields();
	}

	@Override
	public GUIDocument getSelectedDocument() {
		return GridUtil.toDocument(getSelectedRecord());
	}

	@Override
	public int getSelectedIndex() {
		return super.getRecordIndex(getSelectedRecord());
	}

	@Override
	public GUIDocument[] getSelectedDocuments() {
		return GridUtil.toDocuments(getSelectedRecords());
	}

	@Override
	public GUIDocument[] getDocuments() {
		return GridUtil.toDocuments(getRecords());
	}

	@Override
	public void setCanDrag(boolean drag) {
		super.setCanDrag(drag);
		setCanDragRecordsOut(drag);
	}

	@Override
	public int getCount() {
		return getTotalRows();
	}

	@Override
	public void showFilters(boolean showFilters) {
		setShowFilterEditor(showFilters);
	}

	@Override
	public void registerDoubleClickHandler(final DoubleClickHandler handler) {
		DoubleClickHandler passwordCheckingHandler = new DoubleClickHandler() {

			@Override
			public void onDoubleClick(final DoubleClickEvent event) {
				GUIDocument selectedDocument = getSelectedDocument();
				if (selectedDocument == null)
					return;
				if (!selectedDocument.isPasswordProtected())
					handler.onDoubleClick(event);
				else
					DocumentProtectionManager.askForPassword(selectedDocument.getId(), new DocumentProtectionHandler() {
						@Override
						public void onUnprotected(GUIDocument document) {
							handler.onDoubleClick(event);
						}
					});
			}
		};
		addDoubleClickHandler(passwordCheckingHandler);
	}

	@Override
	public void registerSelectionChangedHandler(final SelectionChangedHandler handler) {
		SelectionChangedHandler passwordCheckingHandler = new SelectionChangedHandler() {

			@Override
			public void onSelectionChanged(final SelectionEvent event) {
				GUIDocument selectedDocument = getSelectedDocument();
				if (selectedDocument == null)
					return;
				if (!selectedDocument.isPasswordProtected())
					handler.onSelectionChanged(event);
				else
					DocumentProtectionManager.askForPassword(selectedDocument.getId(), new DocumentProtectionHandler() {
						@Override
						public void onUnprotected(GUIDocument document) {
							handler.onSelectionChanged(event);
						}
					});
			}
		};
		addSelectionChangedHandler(passwordCheckingHandler);
	}

	@Override
	public void registerCellContextClickHandler(final CellContextClickHandler handler) {
		CellContextClickHandler passwordCheckHandler = new CellContextClickHandler() {

			@Override
			public void onCellContextClick(final CellContextClickEvent event) {
				GUIDocument selectedDocument = getSelectedDocument();
				if (selectedDocument == null)
					return;
				if (!selectedDocument.isPasswordProtected())
					handler.onCellContextClick(event);
				else
					DocumentProtectionManager.askForPassword(selectedDocument.getId(), new DocumentProtectionHandler() {
						@Override
						public void onUnprotected(GUIDocument document) {
							handler.onCellContextClick(event);
						}
					});

				if (event != null)
					event.cancel();
			}

		};
		addCellContextClickHandler(passwordCheckHandler);
	}

	@Override
	public void registerDataArrivedHandler(DataArrivedHandler handler) {
		addDataArrivedHandler(handler);
	}

	@Override
	public void selectDocument(long docId) {
		deselectAll();
		RecordList rlist = getDataAsRecordList();
		Record record = rlist.find("id", Long.toString(docId));
		if (record != null)
			selectSingleRecord(record);
	}

	@Override
	public int getSelectedCount() {
		ListGridRecord[] selection = getSelectedRecords();
		if (selection != null && selection.length >= 1)
			return selection.length;
		else
			return 0;
	}

	@Override
	public long[] getSelectedIds() {
		return GridUtil.getIds(getSelectedRecords());
	}

	@Override
	public void expandVisibleRows() {
		Integer[] rows = getVisibleRows();
		if (rows[0] == -1 || rows[1] == -1)
			return;
		for (int i = rows[0]; i < rows[1]; i++)
			expandRecord(getRecord(i));
	}

	@Override
	public void setCanExpandRows() {
		setCanExpandRecords(true);
		setExpansionMode(ExpansionMode.DETAIL_FIELD);
		setDetailField("summary");

	}

	@Override
	protected Canvas getExpansionComponent(final ListGridRecord record) {
		return new HTMLFlow("<div class='details'>"
				+ (record.getAttributeAsString("summary") != null ? record.getAttributeAsString("summary") : "")
				+ "</div>");
	}

	@Override
	public void setDocuments(GUIDocument[] documents) {
		ListGridRecord[] records = new ListGridRecord[0];
		if (documents == null || documents.length == 0)
			setRecords(records);

		records = new ListGridRecord[documents.length];
		for (int i = 0; i < documents.length; i++) {
			GUIDocument doc = documents[i];
			ListGridRecord record = GridUtil.fromDocument(doc);

			records[i] = record;
		}

		setRecords(records);
	}

	@Override
	public long[] getIds() {
		ListGridRecord[] records = getRecords();
		if (records == null || records.length == 0)
			return new long[0];

		long[] ids = new long[records.length];
		for (int j = 0; j < ids.length; j++)
			ids[j] = Long.parseLong(records[j].getAttributeAsString("id"));

		return ids;
	}

	@Override
	public void removeSelectedDocuments() {
		removeSelectedData();
	}

	@Override
	public void setCursor(Cursor cursor) {
		this.cursor = cursor;
	}

	@Override
	public DateDisplayFormat getDateFormatter() {
		return I18N.getDateDisplayFormat(false);
	}

	@Override
	public DateDisplayFormat getDatetimeFormatter() {
		return I18N.getDateDisplayFormat(true);
	}

	@Override
	protected Canvas createRecordComponent(final ListGridRecord record, Integer colNum) {

		String fieldName = this.getFieldName(colNum);

		if (fieldName.equals("statusIcons")) {
			HLayout statusCanvas = new HLayout(3);
			statusCanvas.setHeight(22);
			statusCanvas.setWidth100();
			statusCanvas.setAlign(Alignment.CENTER);

			// Put the bookmark icon
			{
				ImgButton bookmarkedIcon = new ImgButton();
				bookmarkedIcon.setShowDown(false);
				bookmarkedIcon.setShowRollOver(false);
				bookmarkedIcon.setLayoutAlign(Alignment.CENTER);
				bookmarkedIcon.setHeight(16);
				bookmarkedIcon.setWidth(16);

				if (record.getAttribute("bookmarked") != null) {
					Boolean bookmarked = record.getAttributeAsBoolean("bookmarked");
					bookmarkedIcon.setSrc("[SKIN]/" + DocUtil.getBookmarkedIcon(bookmarked));
					if (bookmarked != null && bookmarked) {
						statusCanvas.addMember(bookmarkedIcon);
						bookmarkedIcon.setPrompt(I18N.message("bookmarked"));
					}
				}
			}

			// Put the indexing icon
			{
				ImgButton indexedIcon = new ImgButton();
				indexedIcon.setShowDown(false);
				indexedIcon.setShowRollOver(false);
				indexedIcon.setLayoutAlign(Alignment.CENTER);
				indexedIcon.setHeight(16);
				indexedIcon.setWidth(16);

				if (record.getAttribute("indexed") != null) {
					Integer indexed = record.getAttributeAsInt("indexed");
					indexedIcon.setSrc("[SKIN]/" + DocUtil.getIndexedIcon(indexed));

					if (indexed != null && indexed.intValue() > 0) {
						statusCanvas.addMember(indexedIcon);

						if (indexed.intValue() == Constants.INDEX_INDEXED) {
							indexedIcon.setPrompt(I18N.message("indexed"));
							indexedIcon.addClickHandler(new ClickHandler() {
								public void onClick(ClickEvent event) {
									Long id = record.getAttributeAsLong("id");
									if (Session.get().getCurrentFolder().isDownload())
										WindowUtils.openUrl(Util.downloadURL(id) + "&downloadText=true");
								}
							});
						} else if (indexed.intValue() == Constants.INDEX_SKIP) {
							indexedIcon.setPrompt(I18N.message("notindexable"));
						}
					}
				}
			}

			// Put the status icon
			{
				ImgButton statusIcon = new ImgButton();
				statusIcon.setShowDown(false);
				statusIcon.setShowRollOver(false);
				statusIcon.setLayoutAlign(Alignment.CENTER);
				statusIcon.setHeight(16);
				statusIcon.setWidth(16);

				if (record.getAttribute("status") != null) {
					Integer status = record.getAttributeAsInt("status");
					statusIcon.setSrc("[SKIN]/" + DocUtil.getLockedIcon(status));

					if (status != null && status.intValue() > 0) {
						statusCanvas.addMember(statusIcon);
						if (status == Constants.DOC_CHECKED_OUT || status == Constants.DOC_LOCKED)
							statusIcon.setPrompt(I18N.message("lockedby") + " "
									+ record.getAttributeAsString("lockUser"));
					}
				}
			}

			// Put the immutable icon
			{
				ImgButton immutableIcon = new ImgButton();
				immutableIcon.setShowDown(false);
				immutableIcon.setShowRollOver(false);
				immutableIcon.setLayoutAlign(Alignment.CENTER);
				immutableIcon.setHeight(16);
				immutableIcon.setWidth(16);

				if (record.getAttribute("immutable") != null) {
					Integer immutable = record.getAttributeAsInt("immutable");
					immutableIcon.setSrc("[SKIN]/" + DocUtil.getImmutableIcon(immutable));

					if (immutable != null && immutable.intValue() == 1) {
						statusCanvas.addMember(immutableIcon);
						immutableIcon.setPrompt(I18N.message("immutable"));
					}
				}
			}

			// Put the password protection icon
			{
				ImgButton passwordIcon = new ImgButton();
				passwordIcon.setShowDown(false);
				passwordIcon.setShowRollOver(false);
				passwordIcon.setLayoutAlign(Alignment.CENTER);
				passwordIcon.setHeight(16);
				passwordIcon.setWidth(16);

				if (record.getAttribute("password") != null) {
					Boolean password = record.getAttributeAsBoolean("password");
					passwordIcon.setSrc("[SKIN]/" + DocUtil.getPasswordProtectedIcon(password));

					if (password != null && password.booleanValue()) {
						statusCanvas.addMember(passwordIcon);
						passwordIcon.setPrompt(I18N.message("passwordprotected"));
					}
				}
			}

			// Put the signed icon
			{
				ImgButton signedIcon = new ImgButton();
				signedIcon.setShowDown(false);
				signedIcon.setShowRollOver(false);
				signedIcon.setLayoutAlign(Alignment.CENTER);
				signedIcon.setPrompt(I18N.message("signed"));
				signedIcon.setHeight(16);
				signedIcon.setWidth(16);

				if (record.getAttribute("signed") != null) {
					Integer signed = record.getAttributeAsInt("signed");
					signedIcon.setSrc("[SKIN]/" + DocUtil.getSignedIcon(signed));
					statusCanvas.addMember(signedIcon);
					signedIcon.addClickHandler(new ClickHandler() {

						@Override
						public void onClick(ClickEvent event) {
							Long docId = record.getAttributeAsLong("id");
							if (record.getAttributeAsString("filename") != null
									&& record.getAttributeAsString("filename").toLowerCase().endsWith(".pdf"))
								DocUtil.download(docId, null);
							else
								DocUtil.downloadPdfConversion(docId, null);
						}
					});
				}
			}

			// Put the stamped icon
			{
				ImgButton stampedIcon = new ImgButton();
				stampedIcon.setShowDown(false);
				stampedIcon.setShowRollOver(false);
				stampedIcon.setLayoutAlign(Alignment.CENTER);
				stampedIcon.setPrompt(I18N.message("stamped"));
				stampedIcon.setHeight(16);
				stampedIcon.setWidth(16);

				if (record.getAttribute("stamped") != null) {
					Integer stamped = record.getAttributeAsInt("stamped");
					stampedIcon.setSrc("[SKIN]/" + DocUtil.getStampedIcon(stamped));

					if (stamped != null && stamped.intValue() == 1) {
						statusCanvas.addMember(stampedIcon);
						if (Feature.enabled(Feature.STAMP)) {
							stampedIcon.addClickHandler(new ClickHandler() {
								public void onClick(ClickEvent event) {
									final long id = record.getAttributeAsLong("id");
									String fileVersion = record.getAttribute("fileVersion");

									if (Session.get().getCurrentFolder().isDownload())
										DocUtil.downloadPdfConversion(id, fileVersion);
								}
							});
						}
					}
				}
			}

			return statusCanvas;
		} else {
			return null;
		}

	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
	}

	@Override
	public void onDocumentStored(GUIDocument document) {
		if (folder != null && document.getFolder().getId() == folder.getId()) {
			Record doc = findRecord(document.getId());
			if (doc != null)
				return;

			List<ListGridRecord> rec = new ArrayList<ListGridRecord>();
			rec.add(GridUtil.fromDocument(document));

			Record[] records = getDataAsRecordList().toArray();
			if (records != null && records.length > 0) {
				for (Record record : records)
					rec.add(new ListGridRecord(record));
			}

			cursor.setMessage(I18N.message("showndocuments", Integer.toString(rec.size())));

			setRecords(rec.toArray(new ListGridRecord[0]));
		}
	}

	@Override
	public void onDocumentModified(GUIDocument document) {
		updateDocument(document);
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
	public void onDocumentsDeleted(GUIDocument[] documents) {
		if (documents != null)
			for (GUIDocument doc : documents) {
				try {
					Record record = findRecord(doc.getId());
					if (record != null)
						removeData(record);
				} catch (Throwable t) {

				}
			}
		RecordList recordList = getDataAsRecordList();
		cursor.setMessage(I18N.message("showndocuments", "" + recordList.getLength()));
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		if (folder == null)
			return;

		if (document.getFolder().getId() == folder.getId())
			onDocumentStored(document);
		else
			onDocumentsDeleted(new GUIDocument[] { document });
	}

	private Record findRecord(long docId) {
		Record record = find(new AdvancedCriteria("id", OperatorId.EQUALS, docId));
		if (record == null)
			record = find(new AdvancedCriteria("docref", OperatorId.EQUALS, docId));
		return record;
	}

	@Override
	public void destroy() {
		DocumentController.get().removeObserver(this);
		super.destroy();
	}

	@Override
	protected void finalize() {
		destroy();
	}

	public int getTotalRecords() {
		return totalRecords;
	}

	public void setTotalRecords(int totalRecords) {
		this.totalRecords = totalRecords;
	}
	
	@Override
	public GUIFolder getFolder() {
		return folder;
	}
}