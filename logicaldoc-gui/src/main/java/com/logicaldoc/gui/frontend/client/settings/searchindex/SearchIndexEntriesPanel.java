package com.logicaldoc.gui.frontend.client.settings.searchindex;

import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.logicaldoc.gui.frontend.client.services.SearchEngineService;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows a list of entries stored in the fulltext index
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.4
 */
public class SearchIndexEntriesPanel extends VLayout {

	private DocumentsListGrid entriesGrid;

	private SpinnerItem pageSize;

	private SpinnerItem page;

	private StaticTextItem total;

	private TextItem query;

	public SearchIndexEntriesPanel() {
		pageSize = ItemFactory.newSpinnerItem("max", "display", 100, 10, null);
		pageSize.setWidth(70);
		pageSize.setStep(20);
		pageSize.setSaveOnEnter(true);
		pageSize.setImplicitSave(true);
		pageSize.setHint(I18N.message("elements"));
		pageSize.addChangedHandler(event -> {
			page.setValue(1);
			onSearch();
		});

		page = ItemFactory.newSpinnerItem("page", "page", 1, 1, null);
		page.setHint("");
		page.setSaveOnEnter(true);
		page.setImplicitSave(true);
		page.addChangedHandler(event -> onSearch());

		query = ItemFactory.newTextItem("query", "*:*");
		query.addKeyPressHandler(event -> {
			if (event.getKeyName() == null)
				return;
			if (Constants.KEY_ENTER.equals(event.getKeyName().toLowerCase())) {
				page.setValue(1);
				onSearch();
			}
		});

		total = new StaticTextItem("total");
		total.setShowTitle(false);
		total.setWrap(false);

		ToolStripButton search = new ToolStripButton(I18N.message("search"));
		search.addClickHandler(event -> {
			page.setValue(1);
			onSearch();
		});

		ToolStrip toolstrip = new ToolStrip();
		toolstrip.addFormItem(query);
		toolstrip.addButton(search);
		toolstrip.addSeparator();
		toolstrip.addFormItem(pageSize);
		toolstrip.addFormItem(page);
		toolstrip.addFill();
		toolstrip.addFormItem(total);

		entriesGrid = new SearchIndexEntriesGrid();
		entriesGrid.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		addMember(toolstrip);
		addMember(entriesGrid);
	}

	protected void onSearch() {
		int pageNumber = page.getValueAsInteger();
		int size = pageSize.getValueAsInteger();
		String q = query.getValueAsString();
		LD.contactingServer();

		SearchEngineService.Instance.get().query(q, pageNumber, size, new AsyncCallback<GUIResult>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIResult result) {
				long totalPages = (long) Math
						.ceil((double) result.getEstimatedHits() / (double) pageSize.getValueAsInteger().intValue());
				page.setHint("/" + totalPages);
				page.setMax(totalPages > 0 ? (int) totalPages : 1);
				if (totalPages > 0 && totalPages < page.getValueAsInteger().intValue())
					page.setValue(totalPages);

				/**
				 * Update the cursor. Prepare a stack for 2 sections the Title
				 * with search time and the list of hits
				 */
				NumberFormat format = NumberFormat.getFormat("#.###");
				total.setValue(I18N.message("nresultsinseconds", new String[] { "" + result.getEstimatedHits(),
						format.format((double) Search.get().getTime() / (double) 1000) }));

				entriesGrid.setDocuments(result.getHits());
				LD.clearPrompt();
			}
		});
	}

	@Override
	protected void onDraw() {
		super.onDraw();
		onSearch();
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		ListGridRecord selection = entriesGrid.getSelectedRecord();
		if (selection == null)
			return;

		final Long docId = selection.getAttributeAsLong("id") != null && selection.getAttributeAsLong("id") != 0L
				? selection.getAttributeAsLong("id")
				: null;

		MenuItem openInFolder = new MenuItem();
		openInFolder.setTitle(I18N.message("openinfolder"));
		openInFolder.addClickHandler(event -> DocumentsPanel.get().openInFolder(docId));

		MenuItem deleteEntry = new MenuItem();
		deleteEntry.setTitle(I18N.message("ddelete"));
		deleteEntry.addClickHandler(event -> SC.ask(I18N.message("deleteindexentriesconfirm"), confirm -> {
			if (confirm.booleanValue()) {
				LD.contactingServer();
				SearchEngineService.Instance.get().remove(entriesGrid.getSelectedIds(),
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
								LD.clearPrompt();
							}

							@Override
							public void onSuccess(Void arg) {
								LD.clearPrompt();
								onSearch();
							}
						});
			}
		}));

		openInFolder.setEnabled(selection.getAttributeAsLong("tenantId").longValue() == Session.get().getTenantId());

		contextMenu.setItems(openInFolder, deleteEntry);
		contextMenu.showContextMenu();
	}
}