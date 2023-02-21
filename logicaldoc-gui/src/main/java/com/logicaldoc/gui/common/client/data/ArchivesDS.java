package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle impex archives grid lists. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ArchivesDS extends DataSource {
	public ArchivesDS(int mode, Integer type, Integer status, Long managerId) {
		setTitleField("name");
		setRecordXPath("/list/archive");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField description = new DataSourceTextField("description");

		DataSourceTextField stat = new DataSourceTextField("status");
		DataSourceImageField statusicon = new DataSourceImageField("statusicon");
		DataSourceTextField ttype = new DataSourceTextField("type");
		DataSourceTextField typelabel = new DataSourceTextField("typelabel");
		DataSourceFloatField size = new DataSourceFloatField("size");
		DataSourceTextField creator = new DataSourceTextField("creator");
		DataSourceTextField closer = new DataSourceTextField("closer");
		DataSourceDateTimeField created = new DataSourceDateTimeField("created");
		DataSourceTextField pathonserver = new DataSourceTextField("pathonserver");

		setFields(id, name, description, size, closer, creator, ttype, typelabel, stat, statusicon, created,
				pathonserver);
		setClientOnly(true);
		setDataURL("data/archives.xml?mode=" + mode + "&locale=" + I18N.getLocale()
				+ (status != null ? "&status=" + status : "") + (type != null ? "&type=" + type : "")
				+ (managerId != null ? "&managerId=" + managerId : ""));
	}
}