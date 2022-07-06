package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve ratings. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class RatingsDS extends DataSource {
	public RatingsDS(Long docId) {
		setTitleField("label");
		setRecordXPath("/list/rating");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);

		DataSourceTextField user = new DataSourceTextField("user");
		DataSourceIntegerField vote = new DataSourceIntegerField("vote");
		DataSourceDateField date = new DataSourceDateField("date");

		setFields(id, user, vote, date);
		setDataURL("data/ratings.xml?1=1" + (docId != null ? "&docId=" + docId : ""));
		setClientOnly(true);
	}
}