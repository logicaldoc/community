package com.logicaldoc.gui.frontend.client.dashboard;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUITag;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.search.TagsForm;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Portlet;

/**
 * Portlet specialized in listing history records
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MostUsedTagsPortlet extends Portlet {

	private static final String COUNT = "count";

	private ListGrid list;

	public MostUsedTagsPortlet() {
		if (Feature.enabled(Feature.TAGS)) {
			initGUI();
		} else
			addItem(new FeatureDisabled());
	}

	private void initGUI() {
		setTitle(AwesomeFactory.getIconHtml("tag", I18N.message("mostusedtags")));

		HeaderControl refresh = new HeaderControl(HeaderControl.REFRESH, event -> refresh());

		setHeaderControls(HeaderControls.HEADER_LABEL, refresh);

		setCanDrag(false);
		setCanDrop(false);
		setDragAppearance(DragAppearance.OUTLINE);
		setDragOpacity(30);

		ListGridField word = new ListGridField("word", I18N.message("tag"), 150);
		ListGridField count = new ListGridField(COUNT, I18N.message(COUNT), 60);
		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setWidth100();
		list.setHeight100();
		list.setFields(word, count);
		list.setSelectionType(SelectionStyle.SINGLE);

		HLayout container = new HLayout();
		container.setWidth100();
		container.setHeight100();
		container.setAlign(Alignment.CENTER);
		container.setMargin(10);
		container.addMember(list);

		addItem(container);

		list.addCellDoubleClickHandler(event -> {
			ListGridRecord rec = event.getRecord();
			TagsForm.searchTag(rec.getAttributeAsString("word"), false);
		});

		refresh();
	}

	private void refresh() {
		TagService.Instance.get().getTagCloud(new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<GUITag> cloud) {
				List<ListGridRecord> records = new ArrayList<>();
				for (GUITag tag : cloud) {
					ListGridRecord rec = new ListGridRecord();
					rec.setAttribute("word", tag.getTag());
					rec.setAttribute(COUNT, tag.getCount());
				}
				list.setRecords(records.toArray(new ListGridRecord[0]));
			}
		});
	}
}