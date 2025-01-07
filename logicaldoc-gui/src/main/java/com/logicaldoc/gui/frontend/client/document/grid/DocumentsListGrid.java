package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.DocumentObserver;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.IconGridField;
import com.logicaldoc.gui.common.client.widgets.grid.IntegerListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RatingListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.StatusIconsListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.document.RatingDialog;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderCursor;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.workflow.WorkflowTaskNameListGridField;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DateDisplayFormat;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;

/**
 * Grid used to show a documents list in different contexts.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class DocumentsListGrid extends RefreshableListGrid implements DocumentsGrid, DocumentObserver {

	private static final String LAST_NOTE = "lastNote";

	private static final String GROUP_FIELD_NAME = "group:[{fieldName:";

	private static final String WORKFLOW_STATUS = "workflowStatus";

	private static final String FILENAME = "filename";

	private static final String IMMUTABLE = "immutable";

	private static final String STATUS_ICONS = "statusIcons";

	private static final String TEMPLATE = "template";

	private static final String COMMENT = "comment";

	private static final String PUBLISHED = "published";

	private static final String SUMMARY = "summary";

	private static final String VERSION = "version";

	private static final String PUBLISHED_STATUS = "publishedStatus";

	private static final String STOP_PUBLISHING = "stopPublishing";

	private static final String START_PUBLISHING = "startPublishing";

	private static final String FILE_VERSION = "fileVersion";

	private static final String LANGUAGE = "language";

	private static final String INDEXED = "indexed";

	private static final String CUSTOM_ID = "customId";

	private static final String CREATED = "created";

	private static final String CREATOR = "creator";

	private static final String GUI_AVATAR_SHOWINGRIDS = "gui.avatar.showingrids";

	private static final String PUBLISHER = "publisher";

	private static final String LAST_MODIFIED = "lastModified";

	private static final String PAGES = "pages";

	private static final String RATING = "rating";

	private static final String SHOWNDOCUMENTS = "showndocuments";

	protected Cursor cursor = null;

	protected GUIFolder folder = null;

	// Stores all the possible fields we can use in a grid of documents
	protected Map<String, ListGridField> fieldsMap = new HashMap<>();

	protected String groupField = null;

	/**
	 * The list of all extended attribute names to display
	 */
	protected List<String> extendedAttributes = new ArrayList<>();

	private DocumentsListGrid(GUIFolder folder, List<String> extendedAttributes) {
		this.folder = folder;
		if (extendedAttributes != null)
			this.extendedAttributes = extendedAttributes;
		else
			this.extendedAttributes = Arrays.asList(Session.get().getInfo().getConfig("search.extattr").split(","));

		setEmptyMessage(I18N.message("notitemstoshow"));
		setCanFreezeFields(true);
		setAutoFetchData(true);
		setFilterOnKeypress(true);
		setShowRecordComponents(true);
		setShowRecordComponentsByCell(true);
		setSaveLocally(true);

		if (folder != null)
			setCanDrag(folder.isMove());

		prepareFieldsMap();

		addCellClickHandler(this::onCellClick);

		addDataArrivedHandler(this::onDataArrived);

		addSelectionChangedHandler(selection -> DocumentController.get().setCurrentSelection(getSelectedDocuments()));

		DocumentController.get().addObserver(this);
	}

	private void onDataArrived(DataArrivedEvent event) {
		if (cursor != null)
			cursor.setMessage(I18N.message(SHOWNDOCUMENTS, Integer.toString(getCount())));

		if (Session.get().getHiliteDocId() != null) {
			// Add the sorting by order first
			SortSpecifier specifier = getSortSpecifier("order");
			if (specifier == null) {
				SortSpecifier[] sortSpecs = getSort();
				SortSpecifier[] newSpecs = new SortSpecifier[sortSpecs != null ? sortSpecs.length + 1 : 1];
				newSpecs[0] = new SortSpecifier("order", SortDirection.DESCENDING);
				if (sortSpecs != null)
					for (int i = 1; i < newSpecs.length; i++)
						newSpecs[i] = sortSpecs[i - 1];
				setSort(newSpecs);
			}
			selectDocument(Session.get().getHiliteDocId());
		} else if (groupField != null) {
			groupBy(groupField);
		}
	}

	private void onCellClick(CellClickEvent cellClick) {
		if (RATING.equals(getFieldName(cellClick.getColNum()))) {
			long id = Long.parseLong(getSelectedRecord().getAttribute("id"));
			String ratingImageName = getSelectedRecord().getAttribute(RATING);
			final int docRating = Integer.parseInt(ratingImageName.replace(RATING, ""));
			DocumentService.Instance.get().getRating(id, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIRating rating) {
					if (rating != null) {
						RatingDialog dialog = new RatingDialog(docRating, rating);
						dialog.show();
					}
				}
			});
			cellClick.cancel();
		}
	}

	public DocumentsListGrid(GUIFolder folder) {
		this(folder, null);
	}

	public DocumentsListGrid() {
		this(null, null);
	}

	public DocumentsListGrid(List<String> extendedAttributes) {
		this(null, extendedAttributes);
	}

	/**
	 * Prepares the map that contains all the possible fields we can use
	 */
	protected void prepareFieldsMap() {
		ListGridField id = new ListGridField("id", I18N.getAttributeLabel("id"), 60);
		id.setHidden(true);
		fieldsMap.put(id.getName(), id);

		ListGridField size = new FileSizeListGridField("size", I18N.getAttributeLabel("size"));
		fieldsMap.put(size.getName(), size);

		ListGridField pages = new IntegerListGridField(PAGES, I18N.getAttributeLabel(PAGES));
		pages.setHidden(true);
		fieldsMap.put(pages.getName(), pages);

		ListGridField icon = new IconGridField();
		fieldsMap.put(icon.getName(), icon);

		ListGridField version = new VersionListGridField();
		version.setHidden(true);
		version.setCanFilter(true);
		fieldsMap.put(version.getName(), version);

		ListGridField lastModified = new DateListGridField(LAST_MODIFIED, "lastmodified");
		lastModified.setHidden(true);
		fieldsMap.put(lastModified.getName(), lastModified);

		ListGridField publisher = new UserListGridField(PUBLISHER, "publisherId", PUBLISHER,
				Session.get().getConfigAsBoolean(GUI_AVATAR_SHOWINGRIDS));
		publisher.setTitle(I18N.message(PUBLISHER));
		publisher.setCanFilter(true);
		publisher.setCanSort(true);
		fieldsMap.put(publisher.getName(), publisher);

		DateListGridField published = new DateListGridField(PUBLISHED, PUBLISHED);
		fieldsMap.put(published.getName(), published);

		ListGridField creator = new UserListGridField(CREATOR, "creatorId", CREATOR,
				Session.get().getConfigAsBoolean(GUI_AVATAR_SHOWINGRIDS));
		creator.setCanFilter(true);
		creator.setHidden(true);
		creator.setCanSort(true);
		fieldsMap.put(creator.getName(), creator);

		ListGridField created = new DateListGridField(CREATED, CREATED);
		created.setHidden(true);
		fieldsMap.put(created.getName(), created);

		ListGridField customId = new ColoredListGridField(CUSTOM_ID, I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		fieldsMap.put(customId.getName(), customId);

		ListGridField type = new ColoredListGridField("type", 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		fieldsMap.put(type.getName(), type);

		prepareStatusIcons();

		ListGridField indexed = new ColoredListGridField(INDEXED, " ", 20);
		indexed.setType(ListGridFieldType.IMAGE);
		indexed.setCanSort(false);
		indexed.setAlign(Alignment.CENTER);
		indexed.setShowDefaultContextMenu(false);
		indexed.setImageURLPrefix(Util.imagePrefix());
		indexed.setImageURLSuffix(".png");
		indexed.setCanFilter(false);
		fieldsMap.put(indexed.getName(), indexed);

		FileNameListGridField filename = new FileNameListGridField();
		filename.setCanFilter(true);
		fieldsMap.put(filename.getName(), filename);

		ListGridField lockUserId = new ColoredListGridField("lockUserId", " ", 24);
		lockUserId.setHidden(true);
		lockUserId.setCanFilter(false);
		lockUserId.setCanSort(false);
		fieldsMap.put(lockUserId.getName(), lockUserId);

		ListGridField rating = new RatingListGridField();
		rating.setHidden(true);
		fieldsMap.put(rating.getName(), rating);

		final Map<String, String> languages = I18N.getSupportedLanguages(false);

		ListGridField language = new ColoredListGridField(LANGUAGE, 100);
		language.setType(ListGridFieldType.TEXT);
		language.setCanFilter(false);
		language.setAlign(Alignment.CENTER);
		language.setHidden(true);
		language.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> languages
				.get(rec.getAttribute(LANGUAGE)));
		fieldsMap.put(language.getName(), language);

		ListGridField fileVersion = new VersionListGridField(FILE_VERSION, "fileversion");
		fileVersion.setHidden(false);
		fileVersion.setCanFilter(true);
		fieldsMap.put(fileVersion.getName(), fileVersion);

		ListGridField comment = new ColoredListGridField(COMMENT, 300);
		comment.setHidden(true);
		comment.setCanFilter(true);
		comment.setCanSort(true);
		fieldsMap.put(comment.getName(), comment);

		ListGridField lastNote = new ColoredListGridField(LAST_NOTE, 300);
		lastNote.setHidden(true);
		lastNote.setCanFilter(true);
		lastNote.setCanSort(true);
		fieldsMap.put(lastNote.getName(), lastNote);

		ListGridField tags = new ColoredListGridField("tags", 200);
		tags.setHidden(true);
		tags.setCanFilter(true);
		tags.setCanSort(true);
		fieldsMap.put(tags.getName(), tags);

		ListGridField wfStatus = new WorkflowTaskNameListGridField();
		wfStatus.setHidden(true);
		fieldsMap.put(wfStatus.getName(), wfStatus);

		ListGridField startPublishing = new DateListGridField(START_PUBLISHING, "startpublishing",
				DateCellFormatter.FORMAT_SHORT);
		startPublishing.setHidden(true);
		fieldsMap.put(startPublishing.getName(), startPublishing);

		ListGridField stopPublishing = new DateListGridField(STOP_PUBLISHING, "stoppublishing",
				DateCellFormatter.FORMAT_SHORT);
		stopPublishing.setHidden(true);
		fieldsMap.put(stopPublishing.getName(), stopPublishing);

		ListGridField publishedStatus = new ColoredListGridField(PUBLISHED_STATUS, I18N.message(PUBLISHED), 50);
		publishedStatus.setHidden(true);
		publishedStatus.setCanFilter(true);
		publishedStatus.setCanSort(true);
		fieldsMap.put(publishedStatus.getName(), publishedStatus);

		ListGridField template = new ColoredListGridField(TEMPLATE, 150);
		template.setAlign(Alignment.LEFT);
		template.setHidden(true);
		template.setCanFilter(true);
		template.setCanSort(true);
		fieldsMap.put(template.getName(), template);

		/**
		 * NOTE: If we put the thumbnail the layout of the grid gets corrupted
		 * when filters are activated and the user selects a record so we do not
		 * put the thumbnail image cell
		 */
		ListGridField folderId = new ColoredListGridField("folderId", 80);
		folderId.setHidden(true);
		folderId.setCanSort(false);
		fieldsMap.put(folderId.getName(), folderId);

		ListGridField tenantId = new ColoredListGridField("tenantId", 80);
		tenantId.setHidden(true);
		tenantId.setCanSort(false);
		fieldsMap.put(tenantId.getName(), tenantId);

		// For search only
		ListGridField folderField = new ColoredListGridField("folder", 200);
		folderField.setHidden(true);
		folderField.setCanSort(false);
		fieldsMap.put(folderField.getName(), folderField);

		// For search only
		ListGridField score = new ColoredListGridField("score", 120);
		score.setCanFilter(false);
		score.setHidden(true);
		score.setCellFormatter((value, rec, rowNum, colNum) -> {
			try {
				int scoreValue = rec.getAttributeAsInt("score");
				int red = 100 - scoreValue > 0 ? 100 - scoreValue : 0;
				return "<img src='" + Util.imageUrl("dotblue.gif") + "' style='width: " + scoreValue
						+ "px; height: 8px' title='" + scoreValue + "%'/>" + "<img src='" + Util.imageUrl("dotgrey.gif")
						+ "' style='width: " + red + "px; height: 8px' title='" + scoreValue + "%'/>";
			} catch (Exception e) {
				return "";
			}
		});
		fieldsMap.put(score.getName(), score);

		addExtendedAttributesFields();

		addKeyPressHandler(keyPress -> {
			if (keyPress.isCtrlKeyDown()) {
				List<GUIDocument> selection = getSelectedDocuments();
				if ("C".equalsIgnoreCase(keyPress.getKeyName())) {
					// we could take the action to copy (CTRL + C) into the
					// clipboard
					addSelectionToClipboard(selection);
					Clipboard.getInstance().setLastAction(Clipboard.COPY);
				} else if ("X".equalsIgnoreCase(keyPress.getKeyName())) {
					addSelectionToClipboard(selection);
					Clipboard.getInstance().setLastAction(Clipboard.CUT);
				}
			}
		});
	}

	private void addSelectionToClipboard(List<GUIDocument> selection) {
		for (GUIDocument document : selection)
			Clipboard.getInstance().add(document);
	}

	private void prepareStatusIcons() {
		ListGridField statusIcons = new StatusIconsListGridField();
		fieldsMap.put(statusIcons.getName(), statusIcons);
	}

	private void addExtendedAttributesFields() {
		for (String name : extendedAttributes) {
			if (name != null && !"".equals(name)) {
				ListGridField ext = new ColoredListGridField("ext_" + name,
						Session.get().getInfo().getAttributeLabel(name), 100);
				GUIAttribute attDef = Session.get().getInfo().getAttributeDefinition(name);
				if (attDef != null) {
					if (attDef.getType() == GUIAttribute.TYPE_DATE) {
						ext = new DateListGridField("ext_" + name, Session.get().getInfo().getAttributeLabel(name),
								DateCellFormatter.FORMAT_SHORT);
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
								Session.get().getInfo().getAttributeLabel(name),
								Session.get().getConfigAsBoolean(GUI_AVATAR_SHOWINGRIDS));
						ext.setTitle(Session.get().getInfo().getAttributeLabel(name));
					}
				}

				ext.setHidden(true);
				ext.setCanFilter(true);
				ext.setCanSort(true);
				fieldsMap.put(ext.getName(), ext);
			}
		}
	}

	/**
	 * Merges the given list of fields with the legacy ones.
	 * 
	 * @param fields The fields as you already filled
	 */
	public void mergeFields(List<ListGridField> fields) {
		mergeIconFields(fields);

		if (!fields.contains(fieldsMap.get(FILENAME))) {
			fieldsMap.get(FILENAME).setHidden(true);
			fields.add(fieldsMap.get(FILENAME));
		}

		mergeDateFields(fields);

		if (!fields.contains(fieldsMap.get("type"))) {
			fieldsMap.get("type").setHidden(true);
			fields.add(fieldsMap.get("type"));
		}
		if (!fields.contains(fieldsMap.get("size"))) {
			fieldsMap.get("size").setHidden(true);
			fields.add(fieldsMap.get("size"));
		}
		if (!fields.contains(fieldsMap.get(PAGES))) {
			fieldsMap.get(PAGES).setHidden(true);
			fields.add(fieldsMap.get(PAGES));
		}

		mergeVersionFields(fields);

		mergeUserFields(fields);

		if (!fields.contains(fieldsMap.get(CUSTOM_ID))) {
			fieldsMap.get(CUSTOM_ID).setHidden(true);
			fields.add(fieldsMap.get(CUSTOM_ID));
		}
		if (!fields.contains(fieldsMap.get(RATING))) {
			fieldsMap.get(RATING).setHidden(true);
			fields.add(fieldsMap.get(RATING));
		}
		if (!fields.contains(fieldsMap.get(COMMENT))) {
			fieldsMap.get(COMMENT).setHidden(true);
			fields.add(fieldsMap.get(COMMENT));
		}
		if (!fields.contains(fieldsMap.get(LAST_NOTE))) {
			fieldsMap.get(LAST_NOTE).setHidden(true);
			fields.add(fieldsMap.get(LAST_NOTE));
		}
		if (!fields.contains(fieldsMap.get(WORKFLOW_STATUS))) {
			fieldsMap.get(WORKFLOW_STATUS).setHidden(true);
			fields.add(fieldsMap.get(WORKFLOW_STATUS));
		}
		if (!fields.contains(fieldsMap.get(TEMPLATE))) {
			fieldsMap.get(TEMPLATE).setHidden(true);
			fields.add(fieldsMap.get(TEMPLATE));
		}

		mergePublishingFields(fields);

		if (!fields.contains(fieldsMap.get(LANGUAGE))) {
			fieldsMap.get(LANGUAGE).setHidden(true);
			fields.add(fieldsMap.get(LANGUAGE));
		}
	}

	private void mergePublishingFields(List<ListGridField> fields) {
		if (!fields.contains(fieldsMap.get(START_PUBLISHING))) {
			fieldsMap.get(START_PUBLISHING).setHidden(true);
			fields.add(fieldsMap.get(START_PUBLISHING));
		}

		if (!fields.contains(fieldsMap.get(STOP_PUBLISHING))) {
			fieldsMap.get(STOP_PUBLISHING).setHidden(true);
			fields.add(fieldsMap.get(STOP_PUBLISHING));
		}
	}

	private void mergeUserFields(List<ListGridField> fields) {
		if (!fields.contains(fieldsMap.get(PUBLISHER))) {
			fieldsMap.get(PUBLISHER).setHidden(true);
			fields.add(fieldsMap.get(PUBLISHER));
		}
		if (!fields.contains(fieldsMap.get(CREATOR))) {
			fieldsMap.get(CREATOR).setHidden(true);
			fields.add(fieldsMap.get(CREATOR));
		}
	}

	private void mergeVersionFields(List<ListGridField> fields) {
		if (!fields.contains(fieldsMap.get(FILE_VERSION))) {
			fieldsMap.get(FILE_VERSION).setHidden(true);
			fields.add(fieldsMap.get(FILE_VERSION));
		}
		if (!fields.contains(fieldsMap.get(VERSION))) {
			fieldsMap.get(VERSION).setHidden(true);
			fields.add(fieldsMap.get(VERSION));
		}
	}

	private void mergeDateFields(List<ListGridField> fields) {
		if (!fields.contains(fieldsMap.get(LAST_MODIFIED))) {
			fieldsMap.get(LAST_MODIFIED).setHidden(true);
			fields.add(fieldsMap.get(LAST_MODIFIED));
		}

		if (!fields.contains(fieldsMap.get(CREATED))) {
			fieldsMap.get(CREATED).setHidden(true);
			fields.add(fieldsMap.get(CREATED));
		}

		if (!fields.contains(fieldsMap.get(PUBLISHED))) {
			fieldsMap.get(PUBLISHED).setHidden(true);
			fields.add(fieldsMap.get(PUBLISHED));
		}
	}

	private void mergeIconFields(List<ListGridField> fields) {
		if (!fields.contains(fieldsMap.get("icon")))
			fields.add(0, fieldsMap.get("icon"));

		if (!fields.contains(fieldsMap.get(STATUS_ICONS)))
			fields.add(0, fieldsMap.get(STATUS_ICONS));

		if (!fields.contains(fieldsMap.get("thumbnail")))
			fields.add(0, fieldsMap.get("thumbnail"));
	}

	@Override
	public void deselectAll() {
		deselectAllRecords();
	}

	@Override
	protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
		if (getFieldName(colNum).equals(FILENAME)) {
			int immutable = 0;
			if (rec.getAttribute(IMMUTABLE) != null)
				immutable = rec.getAttributeAsInt(IMMUTABLE);

			if (immutable == 1 || !"yes".equals(rec.getAttribute(PUBLISHED_STATUS))) {
				return "color: #888888; font-style: italic;";
			} else {
				return super.getCellCSSText(rec, rowNum, colNum);
			}
		} else {
			return super.getCellCSSText(rec, rowNum, colNum);
		}
	}

	@Override
	public void updateDocument(GUIDocument document) {
		Record rec = findRecord(document.getId());
		if (rec != null) {
			DocumentGridUtil.updateRecord(document, rec);
			refreshRow(rec);
		}
	}

	private void refreshRow(Record rec) {
		invalidateRecordComponents();
		refreshRecordComponent(getRecordIndex(rec));
		refreshFields();
	}

	@Override
	public GUIDocument getSelectedDocument() {
		return DocumentGridUtil.toDocument(getSelectedRecord());
	}

	@Override
	public int getSelectedIndex() {
		return super.getRecordIndex(getSelectedRecord());
	}

	@Override
	public List<GUIDocument> getSelectedDocuments() {
		return DocumentGridUtil.toDocuments(getSelectedRecords());
	}

	@Override
	public List<GUIDocument> getDocuments() {
		return DocumentGridUtil.toDocuments(getRecords());
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
		addDoubleClickHandler((DoubleClickEvent event) -> {
			GUIDocument selectedDocument = getSelectedDocument();
			if (selectedDocument == null)
				return;
			if (!selectedDocument.isPasswordProtected())
				handler.onDoubleClick(event);
			else
				DocumentProtectionManager.askForPassword(selectedDocument.getId(),
						document -> handler.onDoubleClick(event));
		});
	}

	@Override
	public void registerSelectionChangedHandler(final SelectionChangedHandler handler) {
		addSelectionChangedHandler((SelectionEvent event) -> {
			GUIDocument selectedDocument = getSelectedDocument();
			if (selectedDocument == null)
				return;
			if (!selectedDocument.isPasswordProtected())
				handler.onSelectionChanged(event);
			else
				DocumentProtectionManager.askForPassword(selectedDocument.getId(),
						document -> handler.onSelectionChanged(event));
		});
	}

	@Override
	public void registerCellContextClickHandler(final CellContextClickHandler handler) {
		addCellContextClickHandler((CellContextClickEvent event) -> {
			GUIDocument selectedDocument = getSelectedDocument();
			if (selectedDocument == null)
				return;
			if (!selectedDocument.isPasswordProtected())
				handler.onCellContextClick(event);
			else
				DocumentProtectionManager.askForPassword(selectedDocument.getId(),
						document -> handler.onCellContextClick(event));

			if (event != null)
				event.cancel();
		});
	}

	@Override
	public void registerDataArrivedHandler(DataArrivedHandler handler) {
		addDataArrivedHandler(handler);
	}

	@Override
	public void selectDocument(long docId) {
		deselectAll();
		RecordList rlist = getDataAsRecordList();
		Record rec = rlist.find("id", docId);
		if (rec != null) {
			selectSingleRecord(rec);
			scrollToRow(rlist.indexOf(rec));
			Session.get().setHiliteDocId(null);
			DocumentController.get().selected(getSelectedDocument());
		}
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
	public List<Long> getSelectedIds() {
		return DocumentGridUtil.getIds(getSelectedRecords());
	}

	@Override
	public List<Long> getIds() {
		ListGridRecord[] records = getRecords();
		if (records == null || records.length == 0)
			return new ArrayList<>();

		List<Long> ids = new ArrayList<>();
		for (int i = 0; i < records.length; i++)
			ids.add(Long.parseLong(records[i].getAttributeAsString("id")));

		return ids;
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
		setDetailField(SUMMARY);

	}

	@Override
	protected Canvas getExpansionComponent(final ListGridRecord rec) {
		return new HTMLFlow("<div class='details'>"
				+ (rec.getAttributeAsString(SUMMARY) != null ? rec.getAttributeAsString(SUMMARY) : "") + "</div>");
	}

	@Override
	public void setDocuments(List<GUIDocument> documents) {
		List<ListGridRecord> records = new ArrayList<>();

		for (GUIDocument document : documents)
			records.add(DocumentGridUtil.fromDocument(document));

		setRecords(records.toArray(new ListGridRecord[0]));
	}

	@Override
	public void removeSelectedDocuments() {
		removeSelectedData();
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
	public DateDisplayFormat getDateFormatter() {
		return I18N.getDateDisplayFormat(false);
	}

	@Override
	public DateDisplayFormat getDatetimeFormatter() {
		return I18N.getDateDisplayFormat(true);
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentStored(GUIDocument document) {
		if (folder != null && document.getFolder().getId() == folder.getId()) {
			Record doc = findRecord(document.getId());
			if (doc != null)
				return;

			List<ListGridRecord> rec = new ArrayList<>();
			rec.add(DocumentGridUtil.fromDocument(document));

			Record[] records = getDataAsRecordList().toArray();
			if (records != null && records.length > 0) {
				for (Record recd : records)
					rec.add(new ListGridRecord(recd));
			}

			cursor.setMessage(I18N.message(SHOWNDOCUMENTS, Integer.toString(rec.size())));

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
	public void onDocumentsDeleted(List<GUIDocument> documents) {
		for (GUIDocument doc : documents) {
			try {
				Record rec = findRecord(doc.getId());
				if (rec != null)
					removeData(rec);
			} catch (Exception t) {
				// Nothing to do
			}
		}
		RecordList recordList = getDataAsRecordList();
		cursor.setMessage(I18N.message(SHOWNDOCUMENTS, "" + recordList.getLength()));
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		if (folder == null)
			return;

		if (document.getFolder().getId() == folder.getId())
			onDocumentStored(document);
		else
			onDocumentsDeleted(Arrays.asList(document));
	}

	@Override
	public void onDocumentBeginEditing(GUIDocument document) {
		updateDocument(document);
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		updateDocument(document);
	}

	private Record findRecord(long docId) {
		return find(new AdvancedCriteria("id", OperatorId.EQUALS, docId));
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
		refresh(ds);
	}

	@Override
	public int loadGridLayout(GUIFolder folder) {
		this.folder = folder;
		String previouslySavedState = folder != null ? folder.getGrid() : null;
		if (previouslySavedState == null || previouslySavedState.isEmpty())
			previouslySavedState = Session.get().getUser().getDocsGrid();

		String gridState = DocumentGridUtil.getGridLayoutFromSpec(previouslySavedState);
		Integer pageSize = DocumentGridUtil.getPageSizeFromSpec(previouslySavedState);
		if (pageSize == null)
			pageSize = Session.get().getConfigAsInt("gui.document.pagesize");

		if (gridState != null && !gridState.isEmpty())
			try {
				setViewState(gridState);
			} catch (Exception t) {
				// Nothing to do
			}
		if (getGridCursor() != null) {
			getGridCursor().setPageSize(pageSize);
			if (folder != null)
				getGridCursor().setTotalRecords((int) folder.getDocumentCount());
		}

		/*
		 * Must parse the state to check if there is a grouping field
		 */
		if (gridState != null && gridState.contains(GROUP_FIELD_NAME)) {
			String fieldName = gridState
					.substring(gridState.lastIndexOf(GROUP_FIELD_NAME) + GROUP_FIELD_NAME.length() + 1);
			fieldName = fieldName.substring(0, fieldName.indexOf('\"'));
			groupField = fieldName;
		} else
			groupField = null;

		return pageSize;
	}

	public String getGridLayout() {
		if (getGridCursor() != null)
			return "|" + (Session.get().getConfigAsBoolean("gui.folder.pagination")
					? FolderCursor.get().getCurrentPagination().getPageSize() + "|"
					: "") + getGridCursor().getPageSize() + "|" + getViewState();
		else
			return getViewState();
	}

	public Map<String, ListGridField> getFieldsMap() {
		return fieldsMap;
	}
}