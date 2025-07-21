package com.logicaldoc.gui.frontend.client.menu;

import com.logicaldoc.gui.common.client.Session;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to list the legals confirmed by current user
 * 
 * @author mMarco Meschieri - LogicalDOC
 * @since 9.2.1
 */
public class LegalsDS extends DataSource {

	public LegalsDS() {
		setTitleField("title");
		setRecordXPath("/list/legal");

		DataSourceTextField name = new DataSourceTextField("name");
		name.setPrimaryKey(true);
		name.setRequired(true);
		name.setHidden(true);

		DataSourceTextField category = new DataSourceTextField("category");
		category.setHidden(true);

		DataSourceTextField title = new DataSourceTextField("title");

		DataSourceDateTimeField confirmed = new DataSourceDateTimeField("confirmed");

		setFields(name, category, title, confirmed);
		setClientOnly(true);
		setDataURL("legal?user=" + Session.get().getUser().getUsername());
	}
}