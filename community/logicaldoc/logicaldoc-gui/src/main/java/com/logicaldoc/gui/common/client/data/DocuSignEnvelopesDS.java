package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data Source to handle user's contacts grid lists. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class DocuSignEnvelopesDS extends DataSource {

	public DocuSignEnvelopesDS(Integer max) {
		setTitleField("subject");
		setRecordXPath("/list/envelope");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceTextField subject = new DataSourceTextField("subject");
		subject.setLength(200);
		DataSourceTextField status = new DataSourceTextField("status");
		status.setLength(70);
		DataSourceDateTimeField created = new DataSourceDateTimeField("created");
		created.setLength(110);
		DataSourceDateTimeField expire = new DataSourceDateTimeField("expire");
		expire.setLength(110);
		DataSourceDateTimeField updated = new DataSourceDateTimeField("updated");
		updated.setLength(110);
		updated.setHidden(true);

		setFields(id, subject, status, created, expire, updated);
		setDataURL("data/docusignenvelopes.xml?max=" + (max != null ? max : 40));
		setClientOnly(true);
	}
}