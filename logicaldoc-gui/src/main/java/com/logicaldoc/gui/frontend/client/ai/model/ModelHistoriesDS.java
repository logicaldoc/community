package com.logicaldoc.gui.frontend.client.ai.model;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle model history grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelHistoriesDS extends DataSource {

	public static Integer getDefaultMaxHistories() {
		try {
			return Session.get().getConfigAsInt("gui.maxhistories");
		} catch (Exception t) {
			return 100;
		}
	}

	public ModelHistoriesDS(long modelId) {
		this(modelId, null);
	}

	public ModelHistoriesDS(Long modelId, Integer max) {
		setRecordXPath("/list/history");
		DataSourceTextField user = new DataSourceTextField("user");

		DataSourceDateTimeField date = new DataSourceDateTimeField("date");
		DataSourceTextField evnt = new DataSourceTextField("event");
		DataSourceTextField comment = new DataSourceTextField("comment");
		DataSourceTextField sid = new DataSourceTextField("sid");
		DataSourceTextField key = new DataSourceTextField("key");
		DataSourceTextField ip = new DataSourceTextField("id");
		DataSourceTextField device = new DataSourceTextField("device");
		DataSourceTextField geolocation = new DataSourceTextField("geolocation");

		setFields(user, date, evnt, ip, device, geolocation, comment, sid, key);
		setClientOnly(true);

		String url = "data/ai.xml?object=history&locale=" + I18N.getLocale();
		if (modelId != null)
			url += "&modelId=" + modelId;
		url += "&max=" + (max != null ? max : getDefaultMaxHistories());
		setDataURL(url);
	}
}