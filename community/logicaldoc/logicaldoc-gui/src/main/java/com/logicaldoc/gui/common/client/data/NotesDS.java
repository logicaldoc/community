package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to retrieve notes put by users. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class NotesDS extends DataSource {
	public NotesDS(Long userIdentifier, Long documentIdentifier, Integer pageNumber) {
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
		DataSourceTextField snippet = new DataSourceTextField("snippet");
		DataSourceTextField page = new DataSourceTextField("page");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField docFilename = new DataSourceTextField("docFilename");

		setFields(id, page, userId, user, message, date, docId, docFilename, snippet);
		setClientOnly(true);

		setDataURL("data/notes.xml?1=1" + (userIdentifier != null ? "&" + "userId=" + userIdentifier : "")
				+ (documentIdentifier != null ? "&" + "docId=" + documentIdentifier : "")
				+ (pageNumber != null ? "&" + "page=" + pageNumber : ""));
	}
}