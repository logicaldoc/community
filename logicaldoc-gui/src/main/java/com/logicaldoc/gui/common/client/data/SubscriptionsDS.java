package com.logicaldoc.gui.common.client.data;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve the subscriptions of the current user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SubscriptionsDS extends DataSource {

	public SubscriptionsDS(Long folderId, Long docId) {
		init();
		String dataUrl = "data/subscriptions.xml?1=1";
		if (folderId != null)
			dataUrl += "&folderId=" + folderId;
		if (docId != null)
			dataUrl += "&docId=" + docId;
		setDataURL(dataUrl);
	}

	public SubscriptionsDS(Long folderId, Long userId, String type, String event, int max) {
		init();
		String dataUrl = "data/subscriptions.xml?report=true&max=" + max;
		if (folderId != null)
			dataUrl += "&folderId=" + folderId + "&type=folder";
		if (userId != null)
			dataUrl += "&userId=" + userId;
		if (type != null && folderId == null)
			dataUrl += "&type=" + type;
		if (event != null)
			dataUrl += "&event=" + event;

		setDataURL(dataUrl);
	}

	private void init() {
		setTitleField("name");
		setRecordXPath("/list/subscription");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setRequired(true);

		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceDateTimeField created = new DataSourceDateTimeField("created");
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceTextField objectId = new DataSourceTextField("objectid");
		DataSourceTextField events = new DataSourceTextField("events");
		DataSourceTextField userName = new DataSourceTextField("userName");
		DataSourceTextField userId = new DataSourceTextField("userId");
		DataSourceTextField folderOption = new DataSourceTextField("folderOption");
		DataSourceTextField path = new DataSourceTextField("path");

		setFields(id, icon, name, created, type, objectId, events, userName, userId, folderOption, path);
		setClientOnly(true);
	}
}