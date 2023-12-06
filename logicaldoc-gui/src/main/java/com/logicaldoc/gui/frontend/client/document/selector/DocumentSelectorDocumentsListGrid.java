package com.logicaldoc.gui.frontend.client.document.selector;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.data.DocumentsDSParameters;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridField;

public class DocumentSelectorDocumentsListGrid extends DocumentsListGrid {

	public DocumentSelectorDocumentsListGrid(GUIFolder folder) {
		super(folder);
		setSelectionType(SelectionStyle.MULTIPLE);

		int pageSize = loadGridLayout(folder);
		DocumentsDSParameters params = new DocumentsDSParameters(folder.getId(), null, pageSize, 1,
				DocumentGridUtil.getSortSpec(this));
		DocumentsDS dataSource = new DocumentsDS(params);
		setDataSource(dataSource);

		final List<ListGridField> fields = new ArrayList<>();

		fields.add(fieldsMap.get("id"));
		fields.add(fieldsMap.get("thumbnail"));
		fields.add(fieldsMap.get("statusIcons"));
		fields.add(fieldsMap.get("icon"));

		String[] cols = Session.get().getInfo().getConfig("gui.document.columns").split(",");
		for (String col : cols) {
			ListGridField field = fieldsMap.get(col);
			if (field != null) {
				field.setHidden(false);
				fields.add(field);
			}
		}

		mergeFields(fields);

		setFields(fields.toArray(new ListGridField[0]));
		
		DocumentController.get().removeObserver(this);
	}
}
