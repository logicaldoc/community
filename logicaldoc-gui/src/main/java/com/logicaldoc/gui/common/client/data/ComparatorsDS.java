package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * DataSource to retrieve all the comparators. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class ComparatorsDS extends DataSource {

	private static final String COMPARATOR = "comparator";

	public ComparatorsDS(String inExt) {
		setTitleField("comparators");
		setRecordXPath("/list/comparator");

		DataSourceTextField id = new DataSourceTextField("id", I18N.message("id"));
		id.setPrimaryKey(true);
		DataSourceTextField in = new DataSourceTextField("in", I18N.message("in"));
		DataSourceTextField converter = new DataSourceTextField(COMPARATOR, I18N.message(COMPARATOR));
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled", I18N.message("enabled"));
		DataSourceTextField label = new DataSourceTextField("label", I18N.message(COMPARATOR));

		setFields(id, in, converter, label, enabled);
		setDataURL("data/comparators.xml?1=1" + (inExt != null ? "&in=" + inExt : ""));
		setClientOnly(true);
	}

	public void setComparator(String comparator) {
		setTitleField("comparators");
		setRecordXPath("/list/association");

		DataSourceTextField id = new DataSourceTextField("id", I18N.message("id"));
		id.setPrimaryKey(true);
		id.setHidden(true);
		DataSourceTextField in = new DataSourceTextField("in", I18N.message("in"));
		DataSourceBooleanField selected = new DataSourceBooleanField("selected", I18N.message("selected"));
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled", I18N.message("enabled"));

		setFields(id, in, selected, enabled);
		setDataURL("data/comparators.xml?comparator=" + comparator);
		setClientOnly(true);
	}

}