package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to show the system usage
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class SystemUsageDS extends DataSource {
	public SystemUsageDS() {
		setTitleField("measure");
		setRecordXPath("/list/usage");

		DataSourceTextField measure = new DataSourceTextField("measure");
		measure.setPrimaryKey(true);
		measure.setHidden(true);
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceTextField max = new DataSourceTextField("max");
		DataSourceTextField used = new DataSourceTextField("used");
		DataSourceTextField available = new DataSourceTextField(I18N.message("available"));
		DataSourceIntegerField use = new DataSourceIntegerField(I18N.message("use"));

		setFields(measure, label, max, used, available, use);
		setClientOnly(true);
		setDataURL("data/systemusage.xml?locale=" + I18N.getLocale());
	}
}