package com.logicaldoc.gui.common.client.data;

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
		DataSourceBooleanField read = new DataSourceBooleanField("read");
		DataSourceBooleanField preview = new DataSourceBooleanField("preview");
		DataSourceBooleanField print = new DataSourceBooleanField("print");
		DataSourceBooleanField write = new DataSourceBooleanField("write");
		DataSourceBooleanField delete = new DataSourceBooleanField("delete");
		DataSourceBooleanField add = new DataSourceBooleanField("add");
		DataSourceBooleanField workflow = new DataSourceBooleanField("workflow");
		DataSourceBooleanField sign = new DataSourceBooleanField("sign");
		DataSourceBooleanField importField = new DataSourceBooleanField("import");
		DataSourceBooleanField export = new DataSourceBooleanField("export");
		DataSourceBooleanField immutable = new DataSourceBooleanField("immutable");
		DataSourceBooleanField rename = new DataSourceBooleanField("rename");
		DataSourceBooleanField security = new DataSourceBooleanField("security");
		DataSourceBooleanField archive = new DataSourceBooleanField("archive");
		DataSourceBooleanField download = new DataSourceBooleanField("download");
		DataSourceBooleanField calendar = new DataSourceBooleanField("calendar");
		DataSourceBooleanField subscription = new DataSourceBooleanField("subscription");
		DataSourceBooleanField password = new DataSourceBooleanField("password");
		DataSourceBooleanField move = new DataSourceBooleanField("move");
		DataSourceBooleanField email = new DataSourceBooleanField("email");
		DataSourceBooleanField automation = new DataSourceBooleanField("automation");
		DataSourceBooleanField storage = new DataSourceBooleanField("storage");
		DataSourceBooleanField readingreq = new DataSourceBooleanField("readingreq");
		DataSourceTextField type = new DataSourceTextField("type");

		setFields(entityId, entity, read, preview, print, write, delete, move, add, workflow, sign, importField, export, rename,
				immutable, security, password, archive, type, download, email, calendar, subscription, automation,
				storage, readingreq);
		setClientOnly(true);
		setDataURL("data/acl.xml?id=" + id + "&type=" + objectType + "&locale=" + I18N.getLocale());
	}
}