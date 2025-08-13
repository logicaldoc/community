package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.DocumentSelector;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.metadata.template.AttributeSelector;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
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

	private static final String LABEL = "label";

	private static final String TAKEATTRIBUTES = "takeattributes";

	private static final String CHAIN = "chain";

	private static final String CSV = "csv";

	private static final String ID = "id";

	private static final String DESCRIPTION = "description";

	private static final String NAME = "name";

	private static final String VALUE = "value";

	private static final String METADATA = "metadata";

	private static final String TYPE = "type";

	private DynamicForm form = new DynamicForm();

	private HLayout container = new HLayout();

	private FolderSelector folderSelector;

	private DocumentSelector documentSelector;

	private ListGrid chainGrid;

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

		TextItem label = ItemFactory.newTextItem(LABEL, sampler.getLabel());
		label.addChangedHandler(changedHandler);

		TextItem delimiter = ItemFactory.newTextItem("delimiter", sampler.getDelimiter());
		delimiter.addChangedHandler(changedHandler);
		delimiter.setWidth(30);
		delimiter.setLength(2);
		delimiter.setStartRow(true);
		AdvancedCriteria delimiterCriteria = new AdvancedCriteria(TYPE, OperatorId.EQUALS, CSV);
		delimiter.setVisibleWhen(delimiterCriteria);
		delimiter.setRequiredWhen(delimiterCriteria);

		TextItem quote = ItemFactory.newTextItem("quote", sampler.getQuote());
		quote.addChangedHandler(changedHandler);
		quote.setWidth(30);
		quote.setLength(2);
		quote.setVisibleWhen(delimiterCriteria);

		TextAreaItem description = ItemFactory.newTextAreaItem(DESCRIPTION, sampler.getDescription());
		description.addChangedHandler(changedHandler);
		description.setColSpan(4);
		description.setWidth(400);

		TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", sampler.getAutomation(),
				changedHandler, false);
		automation.addChangedHandler(changedHandler);
		automation.setColSpan(4);
		automation.setWidth(400);
		AdvancedCriteria folderCriteria = new AdvancedCriteria(TYPE, OperatorId.EQUALS, METADATA);
		automation.setVisibleWhen(folderCriteria);

		SelectItem type = ItemFactory.newSelectItem(TYPE);
		type.setOptionDataSource(new SamplerTypeDS());
		type.setValueField(VALUE);
		type.setDisplayField(LABEL);
		type.setValue(sampler.getType());
		type.addChangedHandler(changedHandler);
		type.addChangedHandler(changed -> chainStack.setVisible(CHAIN.equals(type.getValueAsString())));
		type.setRequired(true);
		type.setDisabled(sampler.getId() != 0L);
		type.setVisible(sampler.getId() == 0L);

		StaticTextItem typeValue = ItemFactory.newStaticTextItem("typeValue", TYPE,
				I18N.message("aisamplertype." + sampler.getType()));
		typeValue.setVisible(sampler.getId() != 0L);

		StaticTextItem id = ItemFactory.newStaticTextItem(ID, Long.toString(sampler.getId()));
		id.setVisible(sampler.getId() != 0L);

		folderSelector.setFolder(sampler.getFolder());
		folderSelector.addFolderChangeListener(folder -> changedHandler.onChanged(null));
		folderSelector.setVisibleWhen(folderCriteria);
		documentSelector.setRequiredWhen(folderCriteria);

		documentSelector.setDocument(sampler.getDocument());
		documentSelector.addDocumentChangeListener(document -> changedHandler.onChanged(null));
		AdvancedCriteria documentCriteria = new AdvancedCriteria(TYPE, OperatorId.NOT_IN_SET,
				new String[] { METADATA, CHAIN });
		documentSelector.setVisibleWhen(documentCriteria);
		documentSelector.setRequiredWhen(documentCriteria);

		TextItem category = ItemFactory.newTextItem("category", sampler.getCategory());
		category.addChangedHandler(changedHandler);
		category.setHint(I18N.message("extattrname"));
		category.setShowHintInField(true);
		category.setIconVAlign(VerticalAlignment.CENTER);
		category.setVisibleWhen(folderCriteria);
		FormItemIcon takeAttributeForCategory = prepareTakeAttributeForCategory();
		category.setIcons(takeAttributeForCategory);

		TextItem features = ItemFactory.newTextItem("features", sampler.getFeatures());
		features.addChangedHandler(changedHandler);
		features.setColSpan(4);
		features.setWidth(400);
		features.setHint(I18N.message("extattrnamesseparated"));
		features.setShowHintInField(true);
		features.setIconVAlign(VerticalAlignment.CENTER);
		features.setVisibleWhen(folderCriteria);
		features.setIcons(prepareTakeAttributeForFeatures(category));

		form.setItems(id, typeValue, type, name, label, delimiter, quote, folderSelector, documentSelector, category,
				features, automation, description);

		container.setMembersMargin(3);
		container.addMember(form);

		prepareChain();
	}

	protected FormItemIcon prepareTakeAttributeForCategory() {
		FormItemIcon takeAttributeForCategory = new FormItemIcon();
		takeAttributeForCategory.setName(TAKEATTRIBUTES);
		takeAttributeForCategory.setSrc("[SKIN]/icons/ballot.png");
		takeAttributeForCategory.setPrompt(I18N.message(TAKEATTRIBUTES));
		takeAttributeForCategory.addFormItemClickHandler(click -> new AttributeSelector(
				selection -> click.getItem().setValue(selection[0].getAttributeAsString("name"))).show());
		return takeAttributeForCategory;
	}

	protected FormItemIcon prepareTakeAttributeForFeatures(TextItem category) {
		FormItemIcon takeAttributeForFeatures = new FormItemIcon();
		takeAttributeForFeatures.setName(TAKEATTRIBUTES);
		takeAttributeForFeatures.setSrc("[SKIN]/icons/ballot.png");
		takeAttributeForFeatures.setPrompt(I18N.message(TAKEATTRIBUTES));
		takeAttributeForFeatures.addFormItemClickHandler(click -> new AttributeSelector(selection -> {
			StringBuilder str = new StringBuilder(
					click.getItem().getValue() != null ? click.getItem().getValue().toString() : "");

			for (ListGridRecord listGridRecord : selection) {
				String attrName = listGridRecord.getAttributeAsString("name");
				if (!attrName.equals(category.getValue())) {
					if (str.length() > 0)
						str.append(",");
					str.append(attrName);
				}
			}
			click.getItem().setValue(str.toString());
		}).show());
		return takeAttributeForFeatures;
	}

	private void prepareChain() {
		chainGrid = new ListGrid();
		chainGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		chainGrid.setWidth100();
		chainGrid.setHeight100();
		chainGrid.setEmptyMessage(I18N.message("norecords"));
		chainGrid.setCanSort(false);
		chainGrid.setCanFreezeFields(false);
		chainGrid.setCanGroupBy(false);
		chainGrid.setLeaveScrollbarGap(false);
		chainGrid.setShowHeader(true);
		chainGrid.setSelectionType(SelectionStyle.MULTIPLE);
		chainGrid.setCanEdit(false);
		chainGrid.setShowRowNumbers(true);
		chainGrid.setCanReorderRecords(true);
		chainGrid.setAutoFetchData(true);
		chainGrid.setShowRecordComponents(true);
		chainGrid.setShowRecordComponentsByCell(true);
		chainGrid.addDropCompleteHandler(dropCompleted -> changedHandler.onChanged(null));
		chainGrid.addCellContextClickHandler(event -> {
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

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION));
		description.setCanEdit(false);
		description.setCanSort(false);
		description.setAutoFitWidth(true);
		description.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

		chainGrid.setFields(id, name, type, description);

		SelectItem addSampler = prepareSamplerSelector();

		// Initialize the chain grid
		for (GUISampler chanedSampler : sampler.getChain()) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ID, chanedSampler.getId());
			rec.setAttribute(NAME, chanedSampler.getName());
			rec.setAttribute(TYPE, chanedSampler.getType());
			rec.setAttribute(DESCRIPTION, chanedSampler.getDescription());
			chainGrid.addData(rec);
		}

		DynamicForm buttonsForm = new DynamicForm();
		buttonsForm.setItems(addSampler);

		chainStack.setHeight100();
		chainStack.setVisible(CHAIN.equals(sampler.getType()));

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message(CHAIN) + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);
		section.setItems(chainGrid, buttonsForm);

		chainStack.setSections(section);
		chainStack.draw();

		container.addMember(chainStack);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(click -> {
			chainGrid.removeSelectedData();
			changedHandler.onChanged(null);
		});

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	private SelectItem prepareSamplerSelector() {
		SelectItem addSampler = new SamplerSelector();
		addSampler.addChangedHandler(changed -> {
			ListGridRecord selection = addSampler.getSelectedRecord();

			boolean alreadyInChain = chainGrid
					.find(new AdvancedCriteria(ID, OperatorId.EQUALS, selection.getAttributeAsString(ID))) != null;

			if (!alreadyInChain && selection.getAttributeAsLong(ID) != sampler.getId()) {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute(ID, selection.getAttributeAsLong(ID));
				rec.setAttribute(NAME, selection.getAttribute(NAME));
				rec.setAttribute(TYPE, selection.getAttribute(TYPE));
				rec.setAttribute(DESCRIPTION, selection.getAttribute(DESCRIPTION));
				chainGrid.getRecordList().add(rec);
			}
		});
		addSampler.addChangedHandler(changedHandler);

		return addSampler;
	}

	boolean validate() {
		if (form.validate()) {
			sampler.setName(form.getValueAsString(NAME));
			sampler.setLabel(form.getValueAsString(LABEL));
			sampler.setDescription(form.getValueAsString(DESCRIPTION));
			sampler.setDelimiter(form.getValueAsString("delimiter"));
			sampler.setQuote(form.getValueAsString("quote"));
			sampler.setType(form.getValueAsString(TYPE));
			sampler.setFolder(folderSelector.getFolder());
			sampler.setDocument(documentSelector.getDocument());
			sampler.setCategory(form.getValueAsString("category"));
			sampler.setFeatures(form.getValueAsString("features"));
			sampler.setAutomation(form.getValueAsString("automation"));

			if (CHAIN.equals(sampler.getType()) && Boolean.TRUE.equals(chainGrid.getRecordList().isEmpty())) {
				GuiLog.error("samplerchainempty");
				return false;
			}

			sampler.getChain().clear();
			if (CHAIN.equals(sampler.getType())) {
				com.smartgwt.client.data.Record[] chainRecords = chainGrid.getRecordList().toArray();
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