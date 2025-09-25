package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the ACL of a stamp.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
public class StampAclDS extends DataSource {
	public StampAclDS(long stampId) {
		setTitleField("entity");
		setRecordXPath("/list/ace");
		DataSourceTextField entity = new DataSourceTextField("entity");
		DataSourceTextField entityId = new DataSourceTextField("entityId");
		entityId.setPrimaryKey(true);
		DataSourceBooleanField read = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_READ.toLowerCase());
		DataSourceBooleanField write = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase());

		setFields(entityId, entity, read, write);
		setClientOnly(true);
		setDataURL("data/stampacl.xml?stampId=" + stampId + "&locale=" + I18N.getLocale());
	}
}