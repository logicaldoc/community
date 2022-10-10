package com.logicaldoc.gui.frontend.client.dashboard;

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
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Portlet;

/**
 * Portlet specialized in listing history records
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MostUsedTagsPortlet extends Portlet {

	private ListGrid list;

	private HLayout container = null;

	public MostUsedTagsPortlet() {
		if (Feature.enabled(Feature.TAGS)) {
			initGUI();
		} else
			addItem(new FeatureDisabled());
	}

	private void initGUI() {
		setTitle(AwesomeFactory.getIconHtml("tag", I18N.message("mostusedtags")));

		HeaderControl refresh = new HeaderControl(HeaderControl.REFRESH, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, refresh);

		setCanDrag(false);
		setCanDrop(false);
		setDragAppearance(DragAppearance.OUTLINE);
		setDragOpacity(30);

		ListGridField word = new ListGridField("word", I18N.message("tag"), 150);
		ListGridField count = new ListGridField("count", I18N.message("count"), 60);
		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setWidth100();
		list.setHeight100();
		list.setFields(word, count);
		list.setSelectionType(SelectionStyle.SINGLE);

		container = new HLayout();
		container.setWidth100();
		container.setHeight100();
		container.setAlign(Alignment.CENTER);
		container.setMargin(10);
		container.addMember(list);

		addItem(container);

		list.addCellDoubleClickHandler(new CellDoubleClickHandler() {
			@Override
			public void onCellDoubleClick(CellDoubleClickEvent event) {
				ListGridRecord record = event.getRecord();
				TagsForm.searchTag(record.getAttributeAsString("word"), false);
			}
		});

		refresh();
	}

	private void refresh() {
		TagService.Instance.get().getTagCloud(new AsyncCallback<GUITag[]>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUITag[] cloud) {
				ListGridRecord[] records = new ListGridRecord[cloud.length];
				for (int i = 0; i < cloud.length; i++) {
					records[i] = new ListGridRecord();
					records[i].setAttribute("word", cloud[i].getTag());
					records[i].setAttribute("count", cloud[i].getCount());
				}
				list.setRecords(records);
			}
		});
	}
}