package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.data.NotesDS;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * Portlet specialized in listing the most recent comments of the current user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 6.0
 */
public class NotesDashlet extends DocumentDashlet {

	public NotesDashlet(GUIDashlet guiDashlet) {
		super(guiDashlet);
		setTitle(AwesomeFactory.getIconHtml("sticky-note", I18N.message(guiDashlet.getTitle())));
	}

	@Override
	public String getDocIdAttribute() {
		return "docId";
	}

	@Override
	protected RefreshableListGrid getListGrid() {
		return new RefreshableListGrid();
	}

	@Override
	protected void onDraw() {
		if (Feature.enabled(Feature.NOTES)) {
			super.onDraw();
		} else {
			addItem(new FeatureDisabled());
		}
	}

	@Override
	protected List<ListGridField> prepareGridFields(RefreshableListGrid grid) {
		List<ListGridField> fields = new ArrayList<>();

		ListGridField date = new DateListGridField("date", "date");
		ListGridField title = new ColoredListGridField("title", I18N.message("note"));
		FileNameListGridField filename = new FileNameListGridField();
		filename.setAutoFitWidth(true);

		fields.add(date);
		fields.add(filename);
		fields.add(title);
		return fields;
	}

	@Override
	protected DataSource getDataSource() {
		return new NotesDS(getDataSourceUrl());
	}
}