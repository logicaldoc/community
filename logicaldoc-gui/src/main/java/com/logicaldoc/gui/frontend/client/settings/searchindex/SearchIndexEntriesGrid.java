package com.logicaldoc.gui.frontend.client.settings.searchindex;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * Grid of entries in the fulltext index
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class SearchIndexEntriesGrid extends DocumentsListGrid {

	public SearchIndexEntriesGrid() {
		super();
		setShowRecordComponents(true);
		setShowRecordComponentsByCell(true);
		setSelectionType(SelectionStyle.MULTIPLE);

		fieldsMap.get("type").setHidden(true);
		fieldsMap.get("id").setHidden(false);
		fieldsMap.get("folderId").setHidden(false);
		fieldsMap.get("tenantId").setHidden(false);
		fieldsMap.get("language").setHidden(false);

		List<ListGridField> fields = new ArrayList<ListGridField>();
		fields.add(fieldsMap.get("id"));
		fields.add(fieldsMap.get("thumbnail"));
		fields.add(fieldsMap.get("statusIcons"));
		fields.add(fieldsMap.get("icon"));

		addField("filename", fields);
		addField("lastModified", fields);
		addField("type", fields);
		addField("size", fields);
		addField("pages", fields);
		addField("fileVersion", fields);
		addField("version", fields);
		addField("publisher", fields);
		addField("published", fields);
		addField("creator", fields);
		addField("created", fields);
		addField("customId", fields);
		addField("folder", fields);
		addField("folderId", fields);
		addField("rating", fields);
		addField("comment", fields);
		addField("workflowStatus", fields);
		addField("workflowStatusDisp", fields);
		addField("startPublishing", fields);
		addField("stopPublishing", fields);
		addField("template", fields);
		addField("language", fields);
		addField("tenantId", fields);

		setFields(fields.toArray(new ListGridField[0]));
	}

	private void addField(String name, List<ListGridField> fields) {
		if (!fields.contains(fieldsMap.get(name)))
			fields.add(fieldsMap.get(name));
	}
}