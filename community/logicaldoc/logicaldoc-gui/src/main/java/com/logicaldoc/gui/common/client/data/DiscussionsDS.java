package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle documents grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DiscussionsDS extends DataSource {
	public DiscussionsDS(long docId) {
		setRecordXPath("/list/discussion");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);
		DataSourceTextField user = new DataSourceTextField("user");
		DataSourceIntegerField posts = new DataSourceIntegerField("posts");
		DataSourceIntegerField visits = new DataSourceIntegerField("visits");
		DataSourceDateTimeField lastPost = new DataSourceDateTimeField("lastPost");

		setFields(id, name, user, posts, visits, lastPost);
		setClientOnly(true);
		setDataURL("data/discussions.xml?docId=" + docId);
	}
}