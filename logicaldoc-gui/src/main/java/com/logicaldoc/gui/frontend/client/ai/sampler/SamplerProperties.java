package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.DocumentSelector;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Shows sampler's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class SamplerProperties extends SamplerDetailsTab {

	private static final String ID = "id";

	private static final String DECRIPTION = "decription";

	private static final String NAME = "name";

	private static final String VALUE = "value";

	private static final String METADATA = "metadata";

	private static final String TYPE = "type";

	private DynamicForm form = new DynamicForm();

	private HLayout container = new HLayout();

	private FolderSelector folderSelector;

	private DocumentSelector documentSelector;

	private ListGrid chain;

	private SectionStack chainStack = new SectionStack();

	public SamplerProperties(GUISampler sampler, final ChangedHandler changedHandler) {
		super(sampler, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(container);
		folderSelector = new FolderSelector("folder", null);
		folderSelector.setWidth(250);

		documentSelector = new DocumentSelector("document", null);
		documentSelector.setWidth(250);

		refresh();
	}

	private void refresh() {
		form.clearValues();
		form.clearErrors(false);
		form.destroy();

		if (Boolean.TRUE.equals(container.contains(form)))
			container.removeChild(form);

		form = new DynamicForm();
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem name = ItemFactory.newSimpleTextItem(NAME, sampler.getName());
		name.addChangedHandler(changedHandler);
		name.setRequired(true);

		TextItem label = ItemFactory.newTextItem("label", sampler.getLabel());
		label.addChangedHandler(changedHandler);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", sampler.getDescription());
		description.addChangedHandler(changedHandler);

		TextItem delimiter = ItemFactory.newSimpleTextItem("delimiter", sampler.getDelimiter());
		delimiter.addChangedHandler(changedHandler);
		delimiter.setRequired(true);
		delimiter.setWidth(50);
		delimiter.setLength(1);
		
		TextItem quote = ItemFactory.newSimpleTextItem("quote", sampler.getQuote());
		quote.addChangedHandler(changedHandler);
		quote.setRequired(true);
		quote.setWidth(50);
		quote.setLength(1);
		
		SelectItem type = ItemFactory.newSelectItem(TYPE);
		type.setOptionDataSource(new SamplerTypeDS());
		type.setValueField(VALUE);
		type.setDisplayField(VALUE);
		type.setValue(sampler.getType());
		type.addChangedHandler(changedHandler);
		type.addChangedHandler(changed -> chainStack.setVisible("chain".equals(type.getValueAsString())));
		type.setRequired(true);
		type.setDisabled(sampler.getId() != 0L);
		type.setVisible(sampler.getId() == 0L);

		StaticTextItem typeValue = ItemFactory.newStaticTextItem("typeValue", TYPE, sampler.getType());
		typeValue.setVisible(sampler.getId() != 0L);

		StaticTextItem id = ItemFactory.newStaticTextItem(ID, Long.toString(sampler.getId()));
		id.setVisible(sampler.getId() != 0L);

		folderSelector.setFolder(sampler.getFolder());
		folderSelector.addFolderChangeListener(folder -> changedHandler.onChanged(null));
		folderSelector.setVisibleWhen(new AdvancedCriteria(TYPE, OperatorId.EQUALS, METADATA));

		documentSelector.setDocument(sampler.getDocument());
		documentSelector.addDocumentChangeListener(document -> changedHandler.onChanged(null));
		documentSelector
				.setVisibleWhen(new AdvancedCriteria(TYPE, OperatorId.NOT_IN_SET, new String[] { METADATA, "chain" }));

		form.setItems(id, name, type, typeValue, label, delimiter, quote, folderSelector, documentSelector);

		container.setMembersMargin(3);
		container.addMember(form);

		prepareChain();

	}

	private void prepareChain() {
		chain = new ListGrid();
		chain.setEmptyMessage(I18N.message("notitemstoshow"));
		chain.setWidth100();
		chain.setHeight100();
		chain.setEmptyMessage(I18N.message("norecords"));
		chain.setCanSort(false);
		chain.setCanFreezeFields(false);
		chain.setCanGroupBy(false);
		chain.setLeaveScrollbarGap(false);
		chain.setShowHeader(true);
		chain.setSelectionType(SelectionStyle.MULTIPLE);
		chain.setCanEdit(false);
		chain.setShowRowNumbers(true);
		chain.setCanReorderRecords(true);
		chain.setAutoFetchData(true);
		chain.setShowRecordComponents(true);
		chain.setShowRecordComponentsByCell(true);
		chain.addDropCompleteHandler(dropCompleted -> changedHandler.onChanged(null));
		chain.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField(NAME, I18N.message(NAME));
		name.setCanEdit(false);
		name.setCanSort(false);
		name.setAutoFitWidth(true);
		name.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

		ListGridField type = new ListGridField(TYPE, I18N.message(TYPE));
		type.setCanEdit(false);
		type.setCanSort(false);
		type.setAutoFitWidth(true);
		type.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

		ListGridField description = new ListGridField("description", I18N.message("description"));
		description.setCanEdit(false);
		description.setCanSort(false);
		description.setAutoFitWidth(true);
		description.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

		chain.setFields(id, name, type, description);

		SelectItem addSampler = prepareSamplerSelector();

		// Initialize the chain grid
		for (GUISampler chanedSampler : sampler.getChain()) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ID, chanedSampler.getId());
			rec.setAttribute(NAME, chanedSampler.getName());
			rec.setAttribute(TYPE, chanedSampler.getType());
			rec.setAttribute(DECRIPTION, chanedSampler.getDescription());
			chain.addData(rec);
		}

		DynamicForm buttonsForm = new DynamicForm();
		buttonsForm.setItems(addSampler);

		chainStack.setHeight100();
		chainStack.setVisible("chain".equals(sampler.getType()));

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message("chain") + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);
		section.setItems(chain, buttonsForm);

		chainStack.setSections(section);
		chainStack.draw();

		container.addMember(chainStack);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(click -> {
			chain.removeSelectedData();
			changedHandler.onChanged(null);
		});

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	private SelectItem prepareSamplerSelector() {
		SelectItem addSampler = ItemFactory.newSelectItem("sampler");
		addSampler.setOptionDataSource(new SamplerDS(null));
		addSampler.setValueField(ID);
		addSampler.setDisplayField("name");
		addSampler.setSortField("name");

		ListGridField id = new IdListGridField();
		ListGridField name = new ListGridField(NAME, I18N.message(NAME));
		ListGridField type = new ListGridField(TYPE, I18N.message(TYPE));
		addSampler.setPickListWidth(200);
		addSampler.setPickListFields(id, name, type);

		addSampler.addChangedHandler(changed -> {
			ListGridRecord selection = addSampler.getSelectedRecord();

			boolean alreadyInChain = chain
					.find(new AdvancedCriteria(ID, OperatorId.EQUALS, selection.getAttributeAsString(ID))) != null;

			if (!alreadyInChain && selection.getAttributeAsLong(ID) != sampler.getId()) {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute(ID, selection.getAttributeAsLong(ID));
				rec.setAttribute(NAME, selection.getAttribute(NAME));
				rec.setAttribute(TYPE, selection.getAttribute(TYPE));
				rec.setAttribute(DECRIPTION, selection.getAttribute(DECRIPTION));
				chain.getRecordList().add(rec);
			}
		});
		addSampler.addChangedHandler(changedHandler);

		return addSampler;
	}

	boolean validate() {
		if (form.validate()) {
			sampler.setName(form.getValueAsString(NAME));
			sampler.setLabel(form.getValueAsString("label"));
			sampler.setDescription(form.getValueAsString("description"));
			sampler.setDelimiter(form.getValueAsString("delimiter"));
			sampler.setQuote(form.getValueAsString("quote"));
			sampler.setType(form.getValueAsString(TYPE));
			sampler.setFolder(folderSelector.getFolder());
			sampler.setDocument(documentSelector.getDocument());

			sampler.getChain().clear();
			if ("chain".equals(sampler.getType()) && !chain.getRecordList().isEmpty()) {
				com.smartgwt.client.data.Record[] chainRecords = chain.getRecordList().toArray();
				for (com.smartgwt.client.data.Record chainRecord : chainRecords)
					sampler.getChain()
							.add(new GUISampler(chainRecord.getAttributeAsLong(ID), chainRecord.getAttribute(NAME)));
			}
		}
		return !form.hasErrors();
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