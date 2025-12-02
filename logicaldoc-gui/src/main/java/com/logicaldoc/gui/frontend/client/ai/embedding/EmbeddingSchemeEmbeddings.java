package com.logicaldoc.gui.frontend.client.ai.embedding;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.grid.FileVersionListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.TypeIconGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.preview.PreviewPopup;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows embedding scheme's currently existing embeddings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingSchemeEmbeddings extends EmbeddingSchemeDetailsTab {

	private static final String DOCID = "docid";
	private RefreshableListGrid embeddings;

	public EmbeddingSchemeEmbeddings(EmbeddingSchemesPanel schemesPanel, GUIEmbeddingScheme scheme,
			ChangedHandler changedHandler) {
		super(schemesPanel, scheme, changedHandler);
		setWidth100();
		setHeight100();
	}

	@Override
	protected void onDraw() {
		SpinnerItem max = ItemFactory.newSpinnerItem("max", "", 100, 5, null);
		max.setHint(I18N.message("elements"));
		max.setShowTitle(false);
		max.setStep(10);
		max.addChangedHandler(event -> refresh((Integer) max.getValue()));

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		ToolStripButton display = new ToolStripButton();
		display.setTitle(I18N.message("display"));
		toolStrip.addButton(display);
		toolStrip.addFormItem(max);
		display.addClickHandler(event -> {
			if (Boolean.TRUE.equals(max.validate()))
				refresh((Integer) max.getValue());
		});

		// Prepare a panel containing a title and the documents number
		final InfoPanel infoPanel = new InfoPanel("");

		ListGridField docId = new ColoredListGridField(DOCID);
		docId.setWidth(50);
		docId.setHidden(true);

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));

		ListGridField fileVersion = new FileVersionListGridField();
		fileVersion.setAutoFitWidth(true);
		fileVersion.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

		ListGridField published = new DateListGridField("date", "publishedon");

		ListGridField filename = new FileNameListGridField();

		ListGridField icon = new TypeIconGridField();

		embeddings = new RefreshableListGrid();
		embeddings.setEmptyMessage(I18N.message("notitemstoshow"));
		embeddings.setShowRecordComponents(true);
		embeddings.setShowRecordComponentsByCell(true);
		embeddings.setCanFreezeFields(true);
		embeddings.setAutoFetchData(true);
		embeddings.setSelectionType(SelectionStyle.MULTIPLE);
		embeddings.setShowFilterEditor(true);
		embeddings.setFilterOnKeypress(true);
		embeddings.setDataSource(new EmbeddingsDS(embeddingScheme.getId(), 100));
		embeddings.setFields(docId, icon, filename, size, fileVersion, published);

		embeddings.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		embeddings.addDataArrivedHandler(dataArrivedEvent -> infoPanel
				.setMessage(I18N.message("shownembeddings", Integer.toString(embeddings.getTotalRows()))));

		VLayout contents = new VLayout();
		contents.setMembers(toolStrip, infoPanel, embeddings);

		addMember(contents);
	}

	private void showContextMenu() {
		final ListGridRecord[] selection = embeddings.getSelectedRecords();
		List<Long> docIds = new ArrayList<>();
		for (ListGridRecord rec : selection)
			docIds.add(rec.getAttributeAsLong(DOCID));

		DocumentService.Instance.get().getAllowedPermissions(docIds, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(GUIAccessControlEntry enabledPermissions) {
				Menu contextMenu = new Menu();
				Long selectedEmbeddingDocId = selection[0].getAttributeAsLong(DOCID);
				Long selectedEmbeddingFolderId = selection[0].getAttributeAsLong("folderid");

				MenuItem preview = new MenuItem();
				preview.setTitle(I18N.message("preview"));
				preview.setEnabled(enabledPermissions.isPreview());
				preview.addClickHandler(event -> DocumentService.Instance.get().getById(selectedEmbeddingDocId,
						new DefaultAsyncCallback<>() {
							@Override
							public void handleSuccess(GUIDocument doc) {
								new PreviewPopup(doc).show();
							}
						}));

				MenuItem open = new MenuItem();
				open.setTitle(I18N.message("openinfolder"));
				open.setEnabled(enabledPermissions.isRead());
				open.addClickHandler(
						event -> DocumentsPanel.get().openInFolder(selectedEmbeddingFolderId, selectedEmbeddingDocId));
				open.setEnabled(
						com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DOCUMENTS));

				MenuItem download = new MenuItem();
				download.setTitle(I18N.message("download"));
				download.setEnabled(enabledPermissions.isDownload());
				download.addClickHandler(event -> DocUtil.download(selectedEmbeddingDocId, null));

				MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(
						event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
							if (Boolean.TRUE.equals(confirm)) {
								AIService.Instance.get().removeEmbeddings(embeddingScheme.getId(), docIds,
										new DefaultAsyncCallback<>() {
											@Override
											public void handleSuccess(Void result) {
												embeddings.removeSelectedData();
												embeddings.deselectAllRecords();
												embeddingScheme
														.setEmbeddings(embeddingScheme.getEmbeddings() - docIds.size());
												schemesPanel.updateRecord(embeddingScheme);
											}
										});
							}
						}));

				contextMenu.setItems(preview, download, open, new MenuItemSeparator(), delete);
				contextMenu.showContextMenu();
			}
		});
	}

	private void refresh(Integer max) {
		embeddings.refresh(new EmbeddingsDS(embeddingScheme.getId(), max));
	}

	public boolean validate() {
		return true;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		return prime * result + ((embeddings == null) ? 0 : embeddings.hashCode());
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		EmbeddingSchemeEmbeddings other = (EmbeddingSchemeEmbeddings) obj;
		if (embeddings == null) {
			if (other.embeddings != null)
				return false;
		} else if (!embeddings.equals(other.embeddings))
			return false;
		return true;
	}
}