package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to retrieve notes put by users. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class NotesDS extends DataSource {
	public NotesDS(String url) {
		prepareFields();
		setDataURL(url);
	}

	public NotesDS(Long userIdentifier, Long documentIdentifier, String fileVersion, Integer pageNumber) {
		prepareFields();
		setDataURL("data/notes.xml?1=1" + (userIdentifier != null ? "&" + "userId=" + userIdentifier : "")
				+ (documentIdentifier != null ? "&" + "docId=" + documentIdentifier : "")
				+ (pageNumber != null ? "&" + "page=" + pageNumber : "")
				+ (fileVersion != null ? "&" + "fileVersion=" + fileVersion : ""));
	}

	private void prepareFields() {
		setRecordXPath("/list/post");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceTextField userId = new DataSourceTextField("userId");
		userId.setHidden(true);
		DataSourceTextField docId = new DataSourceTextField("docId");
		docId.setHidden(true);
		DataSourceTextField user = new DataSourceTextField("user");
		DataSourceTextField message = new DataSourceTextField("message");
		DataSourceTextField page = new DataSourceTextField("page");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField docFilename = new DataSourceTextField("filename");
		DataSourceTextField color = new DataSourceTextField("noteColor");
		DataSourceTextField fileVersion = new DataSourceTextField("fileVersion");
		DataSourceImageField icon = new DataSourceImageField("icon");
		icon.setHidden(true);

		setFields(id, page, userId, user, message, date, docId, icon, docFilename, fileVersion, color);
		setClientOnly(true);
	}
}