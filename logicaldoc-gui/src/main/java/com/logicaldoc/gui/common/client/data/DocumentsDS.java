package com.logicaldoc.gui.common.client.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.FieldType;

/**
 * Data Source to handle documents grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsDS extends DataSource {

	public static final Integer DEFAULT_MAX = 100;

	private GUIFolder folder = null;

	public DocumentsDS(GUIFolder folder, String fileFilter, Integer max, int page, Integer indexed, boolean barcoded,
			boolean ocrd, String sortSpec) {
		this(folder != null ? folder.getId() : null, fileFilter, max, page, indexed, barcoded, ocrd, sortSpec);
		this.folder = folder;
	}

	/**
	 * Constructor.
	 * 
	 * @param folderId The folder to be listed (optional)
	 * @param filename A filter on the file nale (optional)
	 * @param max The maximum number of records (if not specified MAX_ROWS is
	 *        used)
	 * @param indexed The indexed flag
	 * @param barcoded The barcoded flag
	 * @param ocrd The ocrd flag
	 * @param sortSpec the sort specification (optional)
	 */
	private DocumentsDS(Long folderId, String fileFilter, Integer max, int page, Integer indexed, boolean barcoded,
			boolean ocrd, String sortSpec) {
		prepareFields(null);

		if (!barcoded && !ocrd) {
			setDataURL("data/documents.xml?locale=" + Session.get().getUser().getLanguage() + "&folderId="
					+ (folderId != null ? folderId : "") + "&filename=" + (fileFilter != null ? fileFilter : "")
					+ "&max=" + (max != null ? max : DEFAULT_MAX) + "&indexed="
					+ (indexed != null ? indexed.toString() : "") + "&page=" + page
					+ (sortSpec != null && !sortSpec.isEmpty() ? "&sort=" + sortSpec : "")
					+ (Session.get().getHiliteDocId() != null ? "&hiliteDocId=" + Session.get().getHiliteDocId() : ""));
		} else if (barcoded)
			setDataURL("data/barcodequeue.xml?max=" + (max != null ? max : DEFAULT_MAX) + "&page=" + page);
		else
			setDataURL("data/zonalocrqueue.xml?max=" + (max != null ? max : DEFAULT_MAX) + "&page=" + page);
	}

	public DocumentsDS(String docIds) {
		prepareFields(null);
		setDataURL("data/documents.xml?docIds=" + docIds);
	}

	public DocumentsDS(int status, int max) {
		prepareFields(null);
		setDataURL("data/documents.xml?status=" + status + "&max=" + max);
	}

	public DocumentsDS(String url, String locale, List<String> extendedAttributes) {
		prepareFields(extendedAttributes);
		setDataURL(url);
	}

	public DocumentsDS(String url, String locale) {
		this(url, locale, null);
	}

	private void prepareFields(List<String> extendedAttributes) {
		setTitleField("filename");
		setRecordXPath("/list/document");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField filename = new DataSourceTextField("filename");

		DataSourceImageField icon = new DataSourceImageField("icon");
		icon.setHidden(true);
		DataSourceTextField customId = new DataSourceTextField("customId");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceTextField version = new DataSourceTextField("version");
		DataSourceTextField docref = new DataSourceTextField("docref");
		docref.setHidden(true);
		DataSourceTextField docrefType = new DataSourceTextField("docrefType");
		docrefType.setHidden(true);
		DataSourceTextField publisher = new DataSourceTextField("publisher");
		DataSourceTextField creator = new DataSourceTextField("creator");
		DataSourceFloatField size = new DataSourceFloatField("size");
		DataSourceIntegerField pages = new DataSourceIntegerField("pages");
		DataSourceDateTimeField lastModified = new DataSourceDateTimeField("lastModified");
		DataSourceDateTimeField published = new DataSourceDateTimeField("published");
		DataSourceDateTimeField created = new DataSourceDateTimeField("created");
		DataSourceIntegerField immutable = new DataSourceIntegerField("immutable");
		DataSourceIntegerField iindexed = new DataSourceIntegerField("indexed");
		DataSourceIntegerField signed = new DataSourceIntegerField("signed");
		DataSourceIntegerField stamped = new DataSourceIntegerField("stamped");
		DataSourceBooleanField bookmarked = new DataSourceBooleanField("bookmarked");
		DataSourceBooleanField password = new DataSourceBooleanField("password");
		DataSourceTextField lockUserId = new DataSourceTextField("lockUserId");
		lockUserId.setHidden(true);
		DataSourceTextField lockUser = new DataSourceTextField("lockUser");
		DataSourceTextField template = new DataSourceTextField("template");
		template.setHidden(true);
		DataSourceTextField fileVersion = new DataSourceTextField("fileVersion");
		DataSourceIntegerField status = new DataSourceIntegerField("status");
		status.setHidden(true);
		DataSourceIntegerField rating = new DataSourceIntegerField("rating");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField wfStatus = new DataSourceTextField("workflowStatus");
		DataSourceTextField wfStatusDisplay = new DataSourceTextField("workflowStatusDisplay");
		wfStatusDisplay.setHidden(true);
		DataSourceTextField publishedStatus = new DataSourceTextField("publishedStatus");
		publishedStatus.setHidden(true);
		DataSourceTextField color = new DataSourceTextField("color");
		color.setHidden(true);
		DataSourceDateTimeField startPublishing = new DataSourceDateTimeField("startPublishing");
		DataSourceDateTimeField stopPublishing = new DataSourceDateTimeField("stopPublishing");
		DataSourceTextField extResId = new DataSourceTextField("extResId");
		DataSourceIntegerField order = new DataSourceIntegerField("order");
		DataSourceTextField language = new DataSourceTextField("language");
		DataSourceTextField tags = new DataSourceTextField("tags");
		DataSourceImageField creatorId = new DataSourceImageField("creatorId", "", 60);
		DataSourceImageField publisherId = new DataSourceImageField("publisherId", "", 60);
		DataSourceImageField tenantId = new DataSourceImageField("tenantId", "", 60);
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");

		List<DataSourceField> fields = new ArrayList<DataSourceField>();
		fields.add(id);
		fields.add(icon);
		fields.add(filename);
		fields.add(type);
		fields.add(size);
		fields.add(pages);
		fields.add(publisherId);
		fields.add(publisher);
		fields.add(version);
		fields.add(docref);
		fields.add(docrefType);
		fields.add(lastModified);
		fields.add(published);
		fields.add(created);
		fields.add(creatorId);
		fields.add(creator);
		fields.add(customId);
		fields.add(immutable);
		fields.add(iindexed);
		fields.add(signed);
		fields.add(stamped);
		fields.add(bookmarked);
		fields.add(password);
		fields.add(lockUserId);
		fields.add(lockUser);
		fields.add(status);
		fields.add(rating);
		fields.add(fileVersion);
		fields.add(comment);
		fields.add(wfStatus);
		fields.add(wfStatusDisplay);
		fields.add(color);
		fields.add(publishedStatus);
		fields.add(startPublishing);
		fields.add(stopPublishing);
		fields.add(extResId);
		fields.add(template);
		fields.add(language);
		fields.add(tags);
		fields.add(order);
		fields.add(tenantId);
		fields.add(date);

		if (extendedAttributes == null) {
			String attrs = Session.get().getInfo().getConfig("search.extattr");
			extendedAttributes = Arrays.asList(attrs.split(","));
		}

		for (String name : extendedAttributes) {
			DataSourceTextField ext = new DataSourceTextField("ext_" + name, name);
			ext.setHidden(true);
			ext.setCanFilter(true);

			GUIAttribute attDef = Session.get().getInfo().getAttributeDefinition(name);

			if (attDef != null) {
				if (attDef.getType() == GUIAttribute.TYPE_DATE) {
					ext.setType(FieldType.DATE);
					ext.setCanFilter(false);
				} else if (attDef.getType() == GUIAttribute.TYPE_INT) {
					ext.setType(FieldType.INTEGER);
					ext.setCanFilter(false);
				} else if (attDef.getType() == GUIAttribute.TYPE_DOUBLE) {
					ext.setType(FieldType.FLOAT);
					ext.setCanFilter(false);
				}
			}

			fields.add(ext);
		}

		setFields(fields.toArray(new DataSourceField[0]));
		setClientOnly(true);
	}

	public GUIFolder getFolder() {
		return folder;
	}
}