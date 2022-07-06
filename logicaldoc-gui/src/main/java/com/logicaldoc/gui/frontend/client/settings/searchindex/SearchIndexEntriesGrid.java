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

		if (!fields.contains(fieldsMap.get("filename")))
			fields.add(fieldsMap.get("filename"));
		if (!fields.contains(fieldsMap.get("lastModified")))
			fields.add(fieldsMap.get("lastModified"));
		if (!fields.contains(fieldsMap.get("type")))
			fields.add(fieldsMap.get("type"));
		if (!fields.contains(fieldsMap.get("size")))
			fields.add(fieldsMap.get("size"));
		if (!fields.contains(fieldsMap.get("pages")))
			fields.add(fieldsMap.get("pages"));
		if (!fields.contains(fieldsMap.get("fileVersion")))
			fields.add(fieldsMap.get("fileVersion"));
		if (!fields.contains(fieldsMap.get("version")))
			fields.add(fieldsMap.get("version"));
		if (!fields.contains(fieldsMap.get("publisher")))
			fields.add(fieldsMap.get("publisher"));
		if (!fields.contains(fieldsMap.get("published")))
			fields.add(fieldsMap.get("published"));
		if (!fields.contains(fieldsMap.get("creator")))
			fields.add(fieldsMap.get("creator"));
		if (!fields.contains(fieldsMap.get("created")))
			fields.add(fieldsMap.get("created"));
		if (!fields.contains(fieldsMap.get("customId")))
			fields.add(fieldsMap.get("customId"));
		if (!fields.contains(fieldsMap.get("folder")))
			fields.add(fieldsMap.get("folder"));
		if (!fields.contains(fieldsMap.get("folderId")))
			fields.add(fieldsMap.get("folderId"));
		if (!fields.contains(fieldsMap.get("rating")))
			fields.add(fieldsMap.get("rating"));
		if (!fields.contains(fieldsMap.get("rating")))
			fields.add(fieldsMap.get("rating"));
		if (!fields.contains(fieldsMap.get("comment")))
			fields.add(fieldsMap.get("comment"));
		if (!fields.contains(fieldsMap.get("workflowStatus")))
			fields.add(fieldsMap.get("workflowStatus"));
		if (!fields.contains(fieldsMap.get("workflowStatusDisp")))
			fields.add(fieldsMap.get("workflowStatusDisp"));
		if (!fields.contains(fieldsMap.get("startPublishing")))
			fields.add(fieldsMap.get("startPublishing"));
		if (!fields.contains(fieldsMap.get("stopPublishing")))
			fields.add(fieldsMap.get("stopPublishing"));
		if (!fields.contains(fieldsMap.get("template")))
			fields.add(fieldsMap.get("template"));
		if (!fields.contains(fieldsMap.get("language")))
			fields.add(fieldsMap.get("language"));
		if (!fields.contains(fieldsMap.get("tenantId")))
			fields.add(fieldsMap.get("tenantId"));

		setFields(fields.toArray(new ListGridField[0]));
	}
}