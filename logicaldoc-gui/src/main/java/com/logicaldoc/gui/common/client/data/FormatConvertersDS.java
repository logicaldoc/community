package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * DataSource to retrieve all the converters. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class FormatConvertersDS extends DataSource {

	private static final String CONVERTER = "converter";

	public FormatConvertersDS(String converter) {
		setTitleField("converters");
		setRecordXPath("/list/association");

		DataSourceTextField id = new DataSourceTextField("id", I18N.message("id"));
		id.setPrimaryKey(true);
		id.setHidden(true);
		DataSourceTextField in = new DataSourceTextField("in", I18N.message("in"));
		DataSourceTextField out = new DataSourceTextField("out", I18N.message("out"));
		DataSourceBooleanField selected = new DataSourceBooleanField("selected", I18N.message("selected"));
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled", I18N.message("enabled"));

		setFields(id, in, out, selected, enabled);
		setDataURL("data/formatconverters.xml?converter=" + converter);
		setClientOnly(true);
	}

	public FormatConvertersDS(String inExt, String outExt) {
		setTitleField("converters");
		setRecordXPath("/list/converter");

		DataSourceTextField id = new DataSourceTextField("id", I18N.message("id"));
		id.setPrimaryKey(true);
		DataSourceTextField in = new DataSourceTextField("in", I18N.message("in"));
		DataSourceTextField out = new DataSourceTextField("out", I18N.message("out"));
		DataSourceTextField converter = new DataSourceTextField(CONVERTER, I18N.message(CONVERTER));
		DataSourceBooleanField enabled = new DataSourceBooleanField("eenabled", I18N.message("enabled"));
		DataSourceTextField label = new DataSourceTextField("label", I18N.message(CONVERTER));

		setFields(id, in, out, converter, label, enabled);
		setDataURL("data/formatconverters.xml?1=1" + (inExt != null ? "&in=" + inExt : "")
				+ (outExt != null ? "&out=" + outExt : ""));
		setClientOnly(true);
	}
}