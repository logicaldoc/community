package com.logicaldoc.gui.frontend.client.folder;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is a form used for quick folder selection
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.4.2
 */
public class FolderSearchDialog extends Window {
	private FolderSearchForm form;

	private ListGridRecord[] lastResult = new ListGridRecord[0];

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
		setPadding(3);
		setMembersMargin(3);

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
		ListGridField description = new ListGridField("description", I18N.message("description"));
		grid.setFields(name, description);
		grid.setSelectionType(SelectionStyle.SINGLE);
		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setShowRecordComponents(true);
		grid.setShowRecordComponentsByCell(true);
		grid.setAutoFetchData(true);
		grid.setWrapCells(false);
		grid.setData(lastResult);

		grid.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				ListGridRecord selection = grid.getSelectedRecord();
				onSelect(selection.getAttributeAsLong("id"), selection.getAttribute("name"));
			}
		});

		VLayout formPanel=new VLayout();
		formPanel.setWidth100();
		formPanel.setHeight(300);
		formPanel.setShowResizeBar(true);
		formPanel.addMember(form);
		
		VLayout gridPanel=new VLayout();
		formPanel.setWidth100();
		gridPanel.setHeight("*");
		gridPanel.addMember(grid);
		
		addItem(formPanel);
		addItem(gridPanel);
	}

	protected void search(GUISearchOptions options) {
		SearchService.Instance.get().search(options, new AsyncCallback<GUIResult>() {
			@Override
			public void onFailure(Throwable caught) {
				ContactingServer.get().hide();
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIResult result) {
				lastResult = new ListGridRecord[result.getHits().length];
				for (int i = 0; i < result.getHits().length; i++) {
					GUIDocument hit = result.getHits()[i];
					ListGridRecord record = new ListGridRecord();
					lastResult[i] = record;
					record.setAttribute("id", hit.getId());
					record.setAttribute("name", hit.getFileName());
					record.setAttribute("description", hit.getSummary());
				}

				if (lastResult.length == 1) {
					onSelect(lastResult[0].getAttributeAsLong("id"), lastResult[0].getAttribute("name"));
				} else
					grid.setData(lastResult);
			}
		});
	}

	public ListGridRecord[] getLastResult() {
		return lastResult;
	}

	public void onSelect(long id, String name) {
		selector.setFolder(id, name);
		destroy();
	}
}