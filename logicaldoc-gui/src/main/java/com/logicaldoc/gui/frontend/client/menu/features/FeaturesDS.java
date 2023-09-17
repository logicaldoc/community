package com.logicaldoc.gui.frontend.client.menu.features;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to display features
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class FeaturesDS extends DataSource {
	public FeaturesDS() {
		setID("LinksDS");

		setTitleField("name");
		setRecordXPath("/list/feature");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField url = new DataSourceTextField("url");

		setFields(name, url);
		setClientOnly(true);

		setDataURL("data/activablefeatures.xml");
		setClientOnly(false);
	}
}