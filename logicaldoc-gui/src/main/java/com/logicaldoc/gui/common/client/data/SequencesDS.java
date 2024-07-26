package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all sequences. It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 68.9.4
 */
public class SequencesDS extends DataSource {
	public SequencesDS(String prefix) {
		setTitleField("name");
		setRecordXPath("/list/sequence");

		DataSourceIntegerField id = new DataSourceIntegerField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceIntegerField value = new DataSourceIntegerField("value");

		setFields(id, name, value);
		setDataURL("data/sequences.xml?prefix=" + prefix);
		setClientOnly(true);
	}
}