package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the possible conversion formats for a specific file.
 * It is based on Xml parsing.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class ConversionFormatsDS extends DataSource {
	public ConversionFormatsDS(String fileName) {
		setTitleField("extension");
		setRecordXPath("/list/format");
		DataSourceTextField extension = new DataSourceTextField("extension", I18N.message("extension"));
		extension.setPrimaryKey(true);

		setFields(extension);
		setDataURL("data/conversionformats.xml?fileName=" + fileName);
		setClientOnly(true);
	}
}