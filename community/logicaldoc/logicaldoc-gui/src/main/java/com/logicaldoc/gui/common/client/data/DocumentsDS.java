package com.logicaldoc.gui.common.client.data;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Session;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data Source to handle documents grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class DocumentsDS extends DataSource {

	private static final Integer DEFAULT_MAX = 100;

	/**
	 * Constructor.
	 * 
	 * @param folderId The folder to be listed (optional)
	 * @param filename A filter on the file nale (optional)
	 * @param max The maximum number of records (if not specified MAX_ROWS is
	 *        used)
	 * @param indexed The indexed flag
	 * @param barcoded The barcoded flag
	 */
	public DocumentsDS(Long folderId, String fileFilter, Integer max, int page, Integer indexed, Integer barcoded) {
		prepareFields();

		if (barcoded == null) {
			String sort = CookiesManager.get(CookiesManager.COOKIE_DOCSLIST_SORT);
			setDataURL("data/documents.xml?locale=" + Session.get().getUser().getLanguage() + "&folderId="
					+ (folderId != null ? folderId : "") + "&filename=" + (fileFilter != null ? fileFilter : "")
					+ "&max=" + (max != null ? max : DEFAULT_MAX) + "&indexed="
					+ (indexed != null ? indexed.toString() : "") + "&page=" + page
					+ (sort != null && !sort.isEmpty() ? "&sort=" + sort : ""));
		} else
			setDataURL("data/tobarcode.xml?max=" + (max != null ? max : DEFAULT_MAX) + "&page=" + page);
	}

	public DocumentsDS(String docIds) {
		prepareFields();
		setDataURL("data/documents.xml?docIds=" + docIds);
	}

	public DocumentsDS(int status, int max) {
		prepareFields();
		setDataURL("data/documents.xml?status=" + status + "&max=" + max);
	}

	private void prepareFields() {
		setTitleField("filename");
		setRecordXPath("/list/document");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField filename = new DataSourceTextField("filename");

		DataSourceImageField icon = new DataSourceImageField("icon");
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
		DataSourceImageField rating = new DataSourceImageField("rating");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField wfStatus = new DataSourceTextField("workflowStatus");
		DataSourceTextField publishedStatus = new DataSourceTextField("publishedStatus");
		publishedStatus.setHidden(true);
		DataSourceDateTimeField startPublishing = new DataSourceDateTimeField("startPublishing");
		DataSourceDateTimeField stopPublishing = new DataSourceDateTimeField("stopPublishing");
		DataSourceTextField extResId = new DataSourceTextField("extResId");

		List<DataSourceField> fields = new ArrayList<DataSourceField>();
		fields.add(id);
		fields.add(icon);
		fields.add(filename);
		fields.add(type);
		fields.add(size);
		fields.add(publisher);
		fields.add(version);
		fields.add(docref);
		fields.add(docrefType);
		fields.add(lastModified);
		fields.add(published);
		fields.add(created);
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
		fields.add(publishedStatus);
		fields.add(startPublishing);
		fields.add(stopPublishing);
		fields.add(extResId);
		fields.add(template);

		String[] extNames = Session.get().getInfo().getConfig("search.extattr").split(",");
		for (String name : extNames) {
			DataSourceTextField ext = new DataSourceTextField("ext_" + name, name);
			ext.setHidden(true);
			ext.setCanFilter(true);
			fields.add(ext);
		}

		setFields(fields.toArray(new DataSourceField[0]));
		setClientOnly(true);
	}
}