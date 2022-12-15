package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager.DocumentProtectionHandler;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.IntegerListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RatingListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.logicaldoc.gui.frontend.client.document.RatingDialog;
import com.logicaldoc.gui.frontend.client.folder.FolderCursor;
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
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.events.KeyDownEvent;
import com.smartgwt.client.widgets.events.KeyDownHandler;
import com.smartgwt.client.widgets.events.KeyPressEvent;
import com.smartgwt.client.widgets.events.KeyPressHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
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

/**
 * Grid used to show a documents list in different contexts.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class DocumentsListGrid extends RefreshableListGrid implements DocumentsGrid, DocumentObserver {

	protected Cursor cursor = null;

	protected GUIFolder folder = null;

	// Stores all the possible fields we can use in a grid of documents
	protected Map<String, ListGridField> fieldsMap = new HashMap<String, ListGridField>();

	protected String groupField = null;

	/**
	 * The list of all extended attribute names to display
	 */
	List<String> extendedAttributes = new ArrayList<String>();

	public DocumentsListGrid(GUIFolder folder, List<String> extendedAttributes) {
		this.folder = folder;
		if (extendedAttributes != null)
			this.extendedAttributes = extendedAttributes;
		else
			this.extendedAttributes = Arrays.asList(Session.get().getInfo().getConfig("search.extattr").split(","));

		this.folder = folder;
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
							GuiLog.serverError(caught);
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
				if (cursor != null)
					cursor.setMessage(I18N.message("showndocuments", Integer.toString(getCount())));

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
		});

		DocumentController.get().addObserver(this);
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

		ListGridField pages = new IntegerListGridField("pages", I18N.getAttributeLabel("pages"));
		pages.setHidden(true);
		fieldsMap.put(pages.getName(), pages);

		ListGridField icon = new ListGridField("icon", " ", 21);
		icon.setType(ListGridFieldType.IMAGE);
		icon.setCanSort(false);
		icon.setCanFilter(false);
		icon.setHidden(true);
		icon.setAlign(Alignment.CENTER);
		icon.setShowDefaultContextMenu(false);
		icon.setImageURLPrefix(Util.imagePrefix());
		icon.setImageURLSuffix(".png");
		fieldsMap.put(icon.getName(), icon);

		ListGridField version = new VersionListGridField();
		version.setHidden(true);
		version.setCanFilter(true);
		fieldsMap.put(version.getName(), version);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");
		lastModified.setHidden(true);
		fieldsMap.put(lastModified.getName(), lastModified);

		ListGridField publisher = new UserListGridField("publisher", "publisherId", "publisher",
				Session.get().getConfigAsBoolean("gui.avatar.showingrids"));
		publisher.setTitle(I18N.message("publisher"));
		publisher.setCanFilter(true);
		publisher.setCanSort(true);
		fieldsMap.put(publisher.getName(), publisher);

		DateListGridField published = new DateListGridField("published", "published");
		fieldsMap.put(published.getName(), published);

		ListGridField creator = new UserListGridField("creator", "creatorId", "creator",
				Session.get().getConfigAsBoolean("gui.avatar.showingrids"));
		creator.setCanFilter(true);
		creator.setHidden(true);
		creator.setCanSort(true);
		fieldsMap.put(creator.getName(), creator);

		ListGridField created = new DateListGridField("created", "created");
		created.setHidden(true);
		fieldsMap.put(created.getName(), created);

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);
		customId.setType(ListGridFieldType.TEXT);
		fieldsMap.put(customId.getName(), customId);

		ListGridField type = new ColoredListGridField("type", I18N.message("type"), 55);
		type.setType(ListGridFieldType.TEXT);
		type.setAlign(Alignment.CENTER);
		fieldsMap.put(type.getName(), type);

		ListGridField statusIcons = new ColoredListGridField("statusIcons", " ");
		statusIcons.setWidth(110);
		statusIcons.setCanFilter(false);
		statusIcons.setCanSort(false);
		fieldsMap.put(statusIcons.getName(), statusIcons);
		statusIcons.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				String color = record.getAttributeAsString("color");

				String content = "<div style='display: flex; text-align: center; justify-content: center;'>";

				// Put the bookmark icon
				{
					if (record.getAttribute("bookmarked") != null) {
						Boolean bookmarked = record.getAttributeAsBoolean("bookmarked");
						if (bookmarked != null && bookmarked)
							content += AwesomeFactory.getIconButtonHTML("bookmark", null, "bookmarked", color, null);
					}
				}

				// Put the indexing icon
				{
					if (record.getAttribute("indexed") != null) {
						Integer indexed = record.getAttributeAsInt("indexed");
						if (indexed != null && indexed.intValue() != Constants.INDEX_TO_INDEX
								&& indexed.intValue() != Constants.INDEX_TO_INDEX_METADATA) {
							Long id = record.getAttributeAsLong("id");
							content += AwesomeFactory.getIndexedIconButtonHTML(id,
									FolderController.get().getCurrentFolder().isDownload(), indexed, color);
						}
					}
				}

				// Put the status icon
				{
					if (record.getAttribute("status") != null) {
						Integer status = record.getAttributeAsInt("status");
						if (status != null && status.intValue() > 0)
							content += AwesomeFactory.getLockedButtonHTML(status,
									record.getAttributeAsString("lockUser"), color);
					}
				}

				// Put the immutable icon
				{
					if (record.getAttribute("immutable") != null) {
						Integer immutable = record.getAttributeAsInt("immutable");
						if (immutable != null && immutable.intValue() == 1)
							content += AwesomeFactory.getIconButtonHTML("hand-paper", null, "immutable", color, null);
					}
				}

				// Put the password protection icon
				{
					if (record.getAttribute("password") != null) {
						Boolean password = record.getAttributeAsBoolean("password");
						if (password != null && password.booleanValue())
							content += AwesomeFactory.getIconButtonHTML("key", null, "passwordprotected", color, null);
					}
				}

				// Put the signed icon
				{
					if (record.getAttribute("signed") != null) {
						Integer signed = record.getAttributeAsInt("signed");
						if (signed != null && signed.intValue() == 1) {
							Long docId = record.getAttributeAsLong("id");
							if (FolderController.get().getCurrentFolder().isDownload())
								content += AwesomeFactory.getIconButtonHTML("badge-check", null, "signed", color,
										(record.getAttributeAsString("filename") != null && record
												.getAttributeAsString("filename").toLowerCase().endsWith(".pdf")
														? Util.downloadURL(docId, null)
														: Util.downloadPdfURL(docId, null)));
							else
								content += AwesomeFactory.getIconButtonHTML("badge-check", null, "signed", color, null);
						}
					}
				}

				// Put the stamped icon
				{
					if (record.getAttribute("stamped") != null) {
						Integer stamped = record.getAttributeAsInt("stamped");
						if (stamped != null && stamped.intValue() == 1) {
							Long docId = record.getAttributeAsLong("id");
							String fileVersion = record.getAttribute("fileVersion");
							if (FolderController.get().getCurrentFolder().isDownload())
								content += AwesomeFactory.getIconButtonHTML("tint", null, "stamped", color,
										(Feature.enabled(Feature.STAMP) ? Util.downloadPdfURL(docId, fileVersion)
												: null));
							else
								content += AwesomeFactory.getIconButtonHTML("tint", null, "stamped", color, null);
						}
					}
				}

				// Put the links icon
				{
					if (record.getAttribute("links") != null) {
						Integer links = record.getAttributeAsInt("links");
						if (links != null && links.intValue() > 0)
							content += AwesomeFactory.getIconButtonHTML("link", null, "withlinks", color, null);
					}
				}

				content += "</div>";
				return content;
			}
		});

		ListGridField indexed = new ColoredListGridField("indexed", " ", 20);
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

		ListGridField rating = new RatingListGridField("rating", I18N.message("rating"));
		rating.setHidden(true);
		fieldsMap.put(rating.getName(), rating);

		final LinkedHashMap<String, String> languages = I18N.getSupportedLanguages(false);

		ListGridField language = new ColoredListGridField("language", I18N.message("language"), 100);
		language.setType(ListGridFieldType.TEXT);
		language.setCanFilter(false);
		language.setAlign(Alignment.CENTER);
		language.setHidden(true);
		language.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return languages.get(record.getAttribute("language"));
			}
		});
		fieldsMap.put(language.getName(), language);

		ListGridField fileVersion = new VersionListGridField("fileVersion", "fileversion");
		fileVersion.setHidden(false);
		fileVersion.setCanFilter(true);
		fieldsMap.put(fileVersion.getName(), fileVersion);

		ListGridField comment = new ColoredListGridField("comment", I18N.message("comment"), 300);
		comment.setHidden(true);
		comment.setCanFilter(true);
		comment.setCanSort(true);
		fieldsMap.put(comment.getName(), comment);

		ListGridField tags = new ColoredListGridField("tags", I18N.message("tags"), 200);
		tags.setHidden(true);
		tags.setCanFilter(true);
		tags.setCanSort(true);
		fieldsMap.put(tags.getName(), tags);

		ListGridField wfStatus = new WorkflowTaskNameListGridField();
		wfStatus.setHidden(true);
		fieldsMap.put(wfStatus.getName(), wfStatus);

		ListGridField startPublishing = new DateListGridField("startPublishing", "startpublishing");
		startPublishing.setHidden(true);
		fieldsMap.put(startPublishing.getName(), startPublishing);

		ListGridField stopPublishing = new DateListGridField("stopPublishing", "stoppublishing");
		stopPublishing.setHidden(true);
		fieldsMap.put(stopPublishing.getName(), stopPublishing);

		ListGridField publishedStatus = new ColoredListGridField("publishedStatus", I18N.message("published"), 50);
		publishedStatus.setHidden(true);
		publishedStatus.setCanFilter(true);
		publishedStatus.setCanSort(true);
		fieldsMap.put(publishedStatus.getName(), publishedStatus);

		ListGridField template = new ColoredListGridField("template", I18N.message("template"), 150);
		template.setAlign(Alignment.LEFT);
		template.setHidden(true);
		template.setCanFilter(true);
		template.setCanSort(false);
		fieldsMap.put(template.getName(), template);

		/**
		 * NOTE: If we put the thumbnail the layout of the grid gets corrupted
		 * when filters are activated and the user selects a record
		 */
//		ListGridField thumbnail = new ListGridField("thumbnail", I18N.message("thumbnail"), 200);
//		thumbnail.setHidden(true);
//		thumbnail.setCanFilter(false);
//		thumbnail.setCellFormatter(new CellFormatter() {
//
//			@Override
//			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
//				try {
//					if (record.getAttribute("docId") != null)
//						return Util.thumbnailImgageHTML(Long.parseLong(record.getAttribute("docId")), null, 200, null);
//					else
//						return Util.thumbnailImgageHTML(Long.parseLong(record.getAttribute("id")), null, 200, null);
//				} catch (Throwable e) {
//					return "";
//				}
//			}
//		});
//      fieldsMap.put(thumbnail.getName(), thumbnail);

		ListGridField folderId = new ColoredListGridField("folderId", I18N.message("folderId"), 80);
		folderId.setHidden(true);
		folderId.setCanSort(false);
		fieldsMap.put(folderId.getName(), folderId);

		ListGridField tenantId = new ColoredListGridField("tenantId", I18N.message("tenantId"), 80);
		tenantId.setHidden(true);
		tenantId.setCanSort(false);
		fieldsMap.put(tenantId.getName(), tenantId);

		// For search only
		ListGridField folder = new ColoredListGridField("folder", I18N.message("folder"), 200);
		folder.setHidden(true);
		folder.setCanSort(false);
		fieldsMap.put(folder.getName(), folder);

		// For search only
		ListGridField score = new ColoredListGridField("score", I18N.message("score"), 120);
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

		for (String name : extendedAttributes) {
			if (name != null && !"".equals(name)) {
				ListGridField ext = new ColoredListGridField("ext_" + name,
						Session.get().getInfo().getAttributeLabel(name), 100);
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
								Session.get().getInfo().getAttributeLabel(name),
								Session.get().getConfigAsBoolean("gui.avatar.showingrids"));
						ext.setTitle(Session.get().getInfo().getAttributeLabel(name));
					}
				}

				ext.setHidden(true);
				ext.setCanFilter(true);
				ext.setCanSort(true);
				fieldsMap.put(ext.getName(), ext);
			}
		}
		
		addKeyPressHandler(new KeyPressHandler() {
			
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if(event.isCtrlKeyDown()) {
					if("C".equals(event.getKeyName())) {
      				   // we could thake the action to copy (CTRL + C) into the clipboard 
					} else if("X".equals(event.getKeyName())) {
						// we could thake the action to cut (CTRL + V)
					}
				}
			}
		});
	}
	
	
	/**
	 * Merges the given list of fields with the legacy ones.
	 * 
	 * @param fields The fields as you already filled
	 */
	public void mergeFields(List<ListGridField> fields) {
		if(!fields.contains(fieldsMap.get("icon")))
			fields.add(0, fieldsMap.get("icon"));
		
		if(!fields.contains(fieldsMap.get("statusIcons")))
			fields.add(0, fieldsMap.get("statusIcons"));
		
		if(!fields.contains(fieldsMap.get("thumbnail")))
			fields.add(0, fieldsMap.get("thumbnail"));
		
		if (!fields.contains(fieldsMap.get("filename"))) {
			fieldsMap.get("filename").setHidden(true);
			fields.add(fieldsMap.get("filename"));
		}
		if (!fields.contains(fieldsMap.get("lastModified"))) {
			fieldsMap.get("lastModified").setHidden(true);
			fields.add(fieldsMap.get("lastModified"));
		}
		if (!fields.contains(fieldsMap.get("type"))) {
			fieldsMap.get("type").setHidden(true);
			fields.add(fieldsMap.get("type"));
		}
		if (!fields.contains(fieldsMap.get("size"))) {
			fieldsMap.get("size").setHidden(true);
			fields.add(fieldsMap.get("size"));
		}
		if (!fields.contains(fieldsMap.get("pages"))) {
			fieldsMap.get("pages").setHidden(true);
			fields.add(fieldsMap.get("pages"));
		}
		if (!fields.contains(fieldsMap.get("fileVersion"))) {
			fieldsMap.get("fileVersion").setHidden(true);
			fields.add(fieldsMap.get("fileVersion"));
		}
		if (!fields.contains(fieldsMap.get("version"))) {
			fieldsMap.get("version").setHidden(true);
			fields.add(fieldsMap.get("version"));
		}
		if (!fields.contains(fieldsMap.get("publisher"))) {
			fieldsMap.get("publisher").setHidden(true);
			fields.add(fieldsMap.get("publisher"));
		}
		if (!fields.contains(fieldsMap.get("published"))) {
			fieldsMap.get("published").setHidden(true);
			fields.add(fieldsMap.get("published"));
		}
		if (!fields.contains(fieldsMap.get("creator"))) {
			fieldsMap.get("creator").setHidden(true);
			fields.add(fieldsMap.get("creator"));
		}
		if (!fields.contains(fieldsMap.get("created"))) {
			fieldsMap.get("created").setHidden(true);
			fields.add(fieldsMap.get("created"));
		}
		if (!fields.contains(fieldsMap.get("customId"))) {
			fieldsMap.get("customId").setHidden(true);
			fields.add(fieldsMap.get("customId"));
		}
		if (!fields.contains(fieldsMap.get("rating"))) {
			fieldsMap.get("rating").setHidden(true);
			fields.add(fieldsMap.get("rating"));
		}
		if (!fields.contains(fieldsMap.get("comment"))) {
			fieldsMap.get("comment").setHidden(true);
			fields.add(fieldsMap.get("comment"));
		}
		if (!fields.contains(fieldsMap.get("workflowStatus"))) {
			fieldsMap.get("workflowStatus").setHidden(true);
			fields.add(fieldsMap.get("workflowStatus"));
		}
		if (!fields.contains(fieldsMap.get("template"))) {
			fieldsMap.get("template").setHidden(true);
			fields.add(fieldsMap.get("template"));
		}
		if (!fields.contains(fieldsMap.get("startPublishing"))) {
			fieldsMap.get("startPublishing").setHidden(true);
			fields.add(fieldsMap.get("startPublishing"));
		}
		if (!fields.contains(fieldsMap.get("stopPublishing"))) {
			fieldsMap.get("stopPublishing").setHidden(true);
			fields.add(fieldsMap.get("stopPublishing"));
		}
		if (!fields.contains(fieldsMap.get("language"))) {
			fieldsMap.get("language").setHidden(true);
			fields.add(fieldsMap.get("language"));
		}
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
			DocumentGridUtil.updateRecord(document, record);
			refreshRow(record);
		}
		
		// Now consider the aliases
		if(document.getDocRef()!=null) {
			record = findRecord(document.getDocRef());
			if (record != null) {
				DocumentGridUtil.updateRecord(document, record);
				refreshRow(record);	
			}
		}
	}

	private void refreshRow(Record record) {
		invalidateRecordComponents();
		refreshRecordComponent(getRecordIndex(record));
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
	public GUIDocument[] getSelectedDocuments() {
		return DocumentGridUtil.toDocuments(getSelectedRecords());
	}

	@Override
	public GUIDocument[] getDocuments() {
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
		Record record = rlist.find("id", docId);
		if (record != null) {
			selectSingleRecord(record);
			scrollToRow(rlist.indexOf(record));
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
	public long[] getSelectedIds() {
		return DocumentGridUtil.getIds(getSelectedRecords());
	}

	@Override
	public Long[] getSelectedIdsAsLong() {
		return DocumentGridUtil.getIdsAsLong(getSelectedRecords());
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

		if (documents != null && documents.length > 0) {
			records = new ListGridRecord[documents.length];
			for (int i = 0; i < documents.length; i++) {
				GUIDocument doc = documents[i];
				ListGridRecord record = DocumentGridUtil.fromDocument(doc);
				records[i] = record;
			}
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

			List<ListGridRecord> rec = new ArrayList<ListGridRecord>();
			rec.add(DocumentGridUtil.fromDocument(document));

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
		if (documents != null) {
			for (GUIDocument doc : documents) {
				try {
					Record record = findRecord(doc.getId());
					if (record != null)
						removeData(record);
				} catch (Throwable t) {
					// Nothing to do
				}
			}
			RecordList recordList = getDataAsRecordList();
			cursor.setMessage(I18N.message("showndocuments", "" + recordList.getLength()));
		}
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

	@Override
	public void onDocumentBeginEditing(GUIDocument document) {
		updateDocument(document);
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		updateDocument(document);
	}

	private Record findRecord(long docId) {
		Record record = find(new AdvancedCriteria("id", OperatorId.EQUALS, docId));
		return record;
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
			} catch (Throwable t) {
				// Nothing to do
			}
		if (getGridCursor() != null) {
			getGridCursor().setPageSize(pageSize);
			if (folder != null)
				getGridCursor().setTotalRecords(folder.getDocumentCount());
		}

		/*
		 * Must parse the state to check if there is a grouping field
		 */
		if (gridState != null && gridState.contains("group:[{fieldName:")) {
			String fieldName = gridState
					.substring(gridState.lastIndexOf("group:[{fieldName:") + "group:[{fieldName:".length() + 1);
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