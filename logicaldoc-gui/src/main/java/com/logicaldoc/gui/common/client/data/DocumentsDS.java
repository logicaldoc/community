package com.logicaldoc.gui.common.client.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
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

	private static final String PAGE = "&page=";

	public static final Integer DEFAULT_MAX = 100;

	/**
	 * Constructor.
	 * 
	 * @param parameters the parameters for the data source
	 */
	public DocumentsDS(DocumentsDSParameters parameters) {
		prepareFields(null);

		if (!parameters.isBarcoded() && !parameters.isOcrd()) {
			setDataURL("data/documents.xml?locale=" + Session.get().getUser().getLanguage() + "&folderId="
					+ defaultValue(parameters.getFolderId()) + "&filename=" + defaultValue(parameters.getFileFilter())
					+ "&max=" + maxValue(parameters.getMax()) + "&indexed=" + defaultValue(parameters.getIndexed())
					+ PAGE + parameters.getPage()
					+ (parameters.getSortSpec() != null && !parameters.getSortSpec().isEmpty()
							? "&sort=" + parameters.getSortSpec()
							: "")
					+ "&hiliteDocId=" + defaultValue(Session.get().getHiliteDocId()));
		} else if (parameters.isBarcoded())
			setDataURL("data/barcodequeue.xml?max=" + maxValue(parameters.getMax()) + PAGE + parameters.getPage());
		else
			setDataURL("data/zonalocrqueue.xml?max=" + maxValue(parameters.getMax()) + PAGE + parameters.getPage());
	}

	private Integer maxValue(Integer max) {
		return max != null ? max : DEFAULT_MAX;
	}

	private Object defaultValue(Object value) {
		return value != null ? value.toString() : "";
	}

	public DocumentsDS(String docIds) {
		prepareFields(null);
		setDataURL("data/documents.xml?docIds=" + docIds);
	}

	public DocumentsDS(int status, int max) {
		prepareFields(null);
		setDataURL("data/documents.xml?status=" + status + "&max=" + max);
	}

	public DocumentsDS(String url, List<String> extendedAttributes) {
		prepareFields(extendedAttributes);
		setDataURL(url);
	}

	private void prepareFields(List<String> extendedAttributes) {
		setTitleField("filename");
		setRecordXPath("/list/document");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField filename = new DataSourceTextField("filename");

		DataSourceTextField icon = new DataSourceTextField("icon");
		icon.setHidden(true);
		DataSourceTextField customId = new DataSourceTextField("customId");
		DataSourceTextField revision = new DataSourceTextField("revision");
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
		DataSourceTextField lastNote = new DataSourceTextField("lastNote");
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

		List<DataSourceField> fields = new ArrayList<>();
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
		fields.add(revision);
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
		fields.add(lastNote);
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
			addExtendedAtrributeField(fields, name);
		}

		setFields(fields.toArray(new DataSourceField[0]));
		setClientOnly(true);
	}

	private void addExtendedAtrributeField(List<DataSourceField> fields, String extAttributeName) {
		DataSourceTextField ext = new DataSourceTextField("ext_" + extAttributeName, extAttributeName);
		ext.setHidden(true);
		ext.setCanFilter(true);

		GUIAttribute attDef = Session.get().getInfo().getAttributeDefinition(extAttributeName);

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
}