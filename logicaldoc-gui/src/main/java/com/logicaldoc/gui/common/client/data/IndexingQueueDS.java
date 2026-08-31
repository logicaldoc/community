package com.logicaldoc.gui.common.client.data;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceImageField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data Source to display the indexing queue
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
public class IndexingQueueDS extends DataSource {

	public static final Integer DEFAULT_MAX = 100;

	public IndexingQueueDS(Integer max) {
		setTitleField("filename");
		setRecordXPath("/list/document");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField filename = new DataSourceTextField("filename");

		DataSourceImageField icon = new DataSourceImageField("icon");
		DataSourceTextField customId = new DataSourceTextField("customId");
		DataSourceTextField version = new DataSourceTextField("version");
		DataSourceTextField docref = new DataSourceTextField("docref");
		docref.setHidden(true);
		DataSourceTextField docrefType = new DataSourceTextField("docrefType");
		docrefType.setHidden(true);
		DataSourceTextField publisher = new DataSourceTextField("publisher");
		DataSourceTextField creator = new DataSourceTextField("creator");
		DataSourceFloatField size = new DataSourceFloatField("size");
		DataSourceDateTimeField lastModified = new DataSourceDateTimeField("lastModified");
		DataSourceDateTimeField published = new DataSourceDateTimeField("published");
		DataSourceDateTimeField created = new DataSourceDateTimeField("created");
		DataSourceIntegerField indexed = new DataSourceIntegerField("indexed");

		List<DataSourceField> fields = new ArrayList<>();
		fields.add(id);
		fields.add(icon);
		fields.add(filename);
		fields.add(size);
		fields.add(publisher);
		fields.add(version);
		fields.add(docref);
		fields.add(docrefType);
		fields.add(lastModified);
		fields.add(published);
		fields.add(created);
		fields.add(creator);
		fields.add(customId);
		fields.add(indexed);

		String[] extNames = Session.get().getInfo().getConfig("search.extattr").split(",");
		for (String name : extNames) {
			DataSourceTextField ext = new DataSourceTextField("ext_" + name, name);
			ext.setHidden(true);
			ext.setCanFilter(true);
			fields.add(ext);
		}

		setFields(fields.toArray(new DataSourceField[0]));
		setClientOnly(true);

		setDataURL("data/indexingqueue.xml?locale=" + Session.get().getUser().getLanguage() + "&max="
				+ (max != null ? max : DEFAULT_MAX));
	}
}