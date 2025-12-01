package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.grid.FileSizeListGridField;
import com.logicaldoc.gui.common.client.grid.IndexedListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.VersionListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows embedding scheme's currently existing embeddings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingSchemeEmbeddings extends EmbeddingSchemeDetailsTab {

	private RefreshableListGrid embeddings;

	public EmbeddingSchemeEmbeddings(GUIEmbeddingScheme scheme, ChangedHandler changedHandler) {
		super(scheme, changedHandler);
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

		ListGridField id = new ColoredListGridField("id");
		id.setWidth(50);
		id.setHidden(true);

		ListGridField size = new FileSizeListGridField("size", I18N.message("size"));
		size.setAlign(Alignment.CENTER);
		size.setType(ListGridFieldType.FLOAT);

		ListGridField version = new VersionListGridField();
		version.setAlign(Alignment.CENTER);

		ListGridField lastModified = new DateListGridField("lastModified", "lastmodified");

		ListGridField publisher = new ColoredListGridField("publisher", I18N.message("publisher"), 90);
		publisher.setAlign(Alignment.CENTER);

		ListGridField published = new DateListGridField("published", "publishedon");

		ListGridField creator = new ColoredListGridField("creator", I18N.message("creator"), 90);
		creator.setAlign(Alignment.CENTER);

		ListGridField created = new DateListGridField("created", "createdon");

		ListGridField customId = new ColoredListGridField("customId", I18N.message("customid"), 110);

		ListGridField indexable = new IndexedListGridField();

		ListGridField filename = new FileNameListGridField();

		ListGridField lockUserId = new ColoredListGridField("lockUserId", " ", 24);
		lockUserId.setHidden(true);

		embeddings = new RefreshableListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord rec, int rowNum, int colNum) {
				if (rec == null)
					return "";
				if (getFieldName(colNum).equals("filename")) {
					if ("stop".equals(rec.getAttribute("immutable"))) {
						return "color: #888888; font-style: italic;";
					} else {
						return super.getCellCSSText(rec, rowNum, colNum);
					}
				} else {
					return super.getCellCSSText(rec, rowNum, colNum);
				}
			}
		};
		embeddings.setEmptyMessage(I18N.message("notitemstoshow"));

		embeddings.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		embeddings.addDataArrivedHandler(dataArrivedEvent -> infoPanel
				.setMessage(I18N.message("shownembeddings", Integer.toString(embeddings.getTotalRows()))));

		embeddings.setShowRecordComponents(true);
		embeddings.setShowRecordComponentsByCell(true);
		embeddings.setCanFreezeFields(true);
		embeddings.setAutoFetchData(true);
		embeddings.setSelectionType(SelectionStyle.MULTIPLE);
		embeddings.setShowFilterEditor(true);
		embeddings.setFilterOnKeypress(true);
		embeddings.setDataSource(new EmbeddingsDS(embeddingScheme.getId(), 100));
		embeddings.setFields(id, indexable, filename, size, lastModified, version, publisher, published, creator,
				created, customId);

		VLayout contents = new VLayout();
		contents.setMembers(toolStrip, infoPanel, embeddings);

		addMember(contents);
	}

	private void showContextMenu() {
		// TODO Auto-generated method stub

	}

	private void refresh(Integer max) {
		embeddings.refresh(new EmbeddingsDS(embeddingScheme.getId(), max));
	}

	public boolean validate() {
		return true;
	}
}