package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is a form used for quick folder selection
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4.2
 */
public class FolderSearchDialog extends Window {
	private static final String DESCRIPTION = "description";

	private FolderSearchForm form;

	private List<ListGridRecord> lastResult = new ArrayList<>();

	private ListGrid grid = new ListGrid();

	private FolderSelector selector;

	public FolderSearchDialog(FolderSelector selector) {
		this.selector = selector;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("folder"));
		setWidth(450);
		setHeight(600);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMembersMargin(0);

		form = new FolderSearchForm() {
			@Override
			protected void search(GUISearchOptions options) {
				FolderSearchDialog.this.search(options);
			}
		};
		form.setWidth100();
		form.setHeight100();

		grid.setWidth100();
		grid.setHeight100();
		grid.setMinHeight(220);
		ListGridField name = new ListGridField("name", I18N.message("name"));
		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION));
		grid.setFields(name, description);
		grid.setSelectionType(SelectionStyle.SINGLE);
		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setShowRecordComponents(true);
		grid.setShowRecordComponentsByCell(true);
		grid.setAutoFetchData(true);
		grid.setWrapCells(false);
		grid.setData(lastResult.toArray(new ListGridRecord[0]));

		grid.addDoubleClickHandler((DoubleClickEvent event) -> {
			ListGridRecord selection = grid.getSelectedRecord();
			onSelect(selection.getAttributeAsLong("id"), selection.getAttribute("name"));
		});

		VLayout formPanel = new VLayout();
		formPanel.setWidth100();
		formPanel.setHeight(300);
		formPanel.setShowResizeBar(true);
		formPanel.addMember(form);

		VLayout gridPanel = new VLayout();
		formPanel.setWidth100();
		gridPanel.setHeight("*");
		gridPanel.addMember(grid);

		addItem(formPanel);
		addItem(gridPanel);
	}

	protected void search(GUISearchOptions options) {
		SearchService.Instance.get().search(options, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIResult result) {
				lastResult = new ArrayList<>();
				for (GUIDocument hit : result.getHits()) {
					ListGridRecord rec = new ListGridRecord();
					rec.setAttribute("id", hit.getId());
					rec.setAttribute("name", hit.getFileName());
					rec.setAttribute(DESCRIPTION, hit.getSummary());
					lastResult.add(rec);
				}

				if (lastResult.size() == 1) {
					onSelect(lastResult.get(0).getAttributeAsLong("id"), lastResult.get(0).getAttribute("name"));
				} else
					grid.setData(lastResult.toArray(new ListGridRecord[0]));
			}
		});
	}

	public List<ListGridRecord> getLastResult() {
		return lastResult;
	}

	public void onSelect(long id, String name) {
		selector.setFolder(id, name);
		destroy();
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}