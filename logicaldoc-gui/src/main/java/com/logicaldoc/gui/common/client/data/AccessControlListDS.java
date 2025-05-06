package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to handle rights on an object. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class AccessControlListDS extends DataSource {

	/**
	 * Constructor
	 * 
	 * @param id identifier of the object
	 * @param objectType type of object(folder, menu, document, template)
	 */
	public AccessControlListDS(long id, String objectType) {
		setTitleField("entity");
		setRecordXPath("/list/ace");
		DataSourceTextField entity = new DataSourceTextField("entity");
		DataSourceTextField entityId = new DataSourceTextField("entityId");
		entityId.setPrimaryKey(true);
		DataSourceBooleanField read = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_READ.toLowerCase());
		DataSourceBooleanField preview = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_PREVIEW.toLowerCase());
		DataSourceBooleanField print = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_PRINT.toLowerCase());
		DataSourceBooleanField write = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase());
		DataSourceBooleanField delete = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_DELETE.toLowerCase());
		DataSourceBooleanField add = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_ADD.toLowerCase());
		DataSourceBooleanField workflow = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_WORKFLOW.toLowerCase());
		DataSourceBooleanField sign = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_SIGN.toLowerCase());
		DataSourceBooleanField importField = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_IMPORT.toLowerCase());
		DataSourceBooleanField export = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_EXPORT.toLowerCase());
		DataSourceBooleanField immutable = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_IMMUTABLE.toLowerCase());
		DataSourceBooleanField rename = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_RENAME.toLowerCase());
		DataSourceBooleanField security = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_SECURITY.toLowerCase());
		DataSourceBooleanField archive = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_ARCHIVE.toLowerCase());
		DataSourceBooleanField download = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_DOWNLOAD.toLowerCase());
		DataSourceBooleanField calendar = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_CALENDAR.toLowerCase());
		DataSourceBooleanField subscription = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_SUBSCRIPTION.toLowerCase());
		DataSourceBooleanField password = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_PASSWORD.toLowerCase());
		DataSourceBooleanField move = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_MOVE.toLowerCase());
		DataSourceBooleanField email = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_EMAIL.toLowerCase());
		DataSourceBooleanField automation = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_AUTOMATION.toLowerCase());
		DataSourceBooleanField store = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_STORE.toLowerCase());
		DataSourceBooleanField readingreq = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_READINGREQ.toLowerCase());
		DataSourceBooleanField customid = new DataSourceBooleanField(GUIAccessControlEntry.PERMISSION_CUSTOMID);
		DataSourceTextField type = new DataSourceTextField("type");

		setFields(entityId, entity, read, preview, print, write, customid, delete, move, add, workflow, sign, importField, export, rename,
				immutable, security, password, archive, type, download, email, calendar, subscription, automation,
				store, readingreq);
		setClientOnly(true);
		setDataURL("data/acl.xml?id=" + id + "&type=" + objectType + "&locale=" + I18N.getLocale());
	}
}