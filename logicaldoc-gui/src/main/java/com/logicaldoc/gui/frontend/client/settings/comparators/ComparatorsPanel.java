package com.logicaldoc.gui.frontend.client.settings.comparators;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.ComparatorsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the comparators settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class ComparatorsPanel extends AdminPanel {

	private static final String LABEL = "label";

	private static final String VALUE = "value";

	private static final String EENABLED = "eenabled";

	protected String gridAttributeName = "comparator";

	protected String settingsPrefix = gridAttributeName + ".";

	protected String listGridAttributeLabel = I18N.message(gridAttributeName);

	// Associations between file formats and a comparator
	protected ListGrid associationsGrid;

	// Settings for the different comparators
	protected ListGrid settingsGrid;

	protected String settingsGridTitle = I18N.message("comparators");

	public ComparatorsPanel(String title) {
		super(title);
	}

	public ComparatorsPanel() {
		super("comparators");
	}

	@Override
	protected void onDraw() {
		ToolStrip toolStrip = prepareToolStrip();

		toolStrip.addFill();

		body.addMember(toolStrip);

		prepareAssociationsGrid();
		SectionStackSection associationsSection = new SectionStackSection(I18N.message("associations"));
		associationsSection.setCanCollapse(false);
		associationsSection.setExpanded(true);
		associationsSection.setItems(associationsGrid);

		ImgButton configButton = new ImgButton();
		configButton.setSrc("[SKIN]/cog.png");
		configButton.setSize(16);
		configButton.setShowFocused(false);
		configButton.setShowRollOver(false);
		configButton.setShowDown(false);
		configButton.addClickHandler((ClickEvent event) -> {
			ComparatorAssociationsDialog dialog = getAssociationsDialog();
			dialog.show();
		});
		if (!Session.get().isDemo())
			associationsSection.setControls(configButton);

		SectionStack associationsStack = new SectionStack();
		associationsStack.setWidth(350);
		associationsStack.setHeight100();
		associationsStack.setSections(associationsSection);

		prepareSettingsGrid();
		SectionStackSection settingsSection = new SectionStackSection(settingsGridTitle);
		settingsSection.setCanCollapse(false);
		settingsSection.setExpanded(true);
		settingsSection.setItems(settingsGrid);
		SectionStack settingsStack = new SectionStack();
		settingsStack.setWidth(500);
		settingsStack.setHeight100();
		settingsStack.setSections(settingsSection);

		HLayout layout = new HLayout();
		layout.setWidth100();
		layout.setHeight100();
		layout.setMembersMargin(3);
		layout.setMembers(associationsStack, settingsStack);

		body.addMember(layout);
	}

	protected ComparatorAssociationsDialog getAssociationsDialog() {
		return new ComparatorAssociationsDialog(associationsGrid);
	}

	protected ToolStrip prepareToolStrip() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.setDisabled(Session.get().isDemo());
		save.addClickHandler(event -> onSave());
		save.setDisabled(Session.get().isDemo());
		toolStrip.addButton(save);
		return toolStrip;
	}

	protected void prepareAssociationsGrid() {
		buildAssociationsGrid();

		ListGridField in = new ListGridField("in", I18N.message("ext"), 40);
		ListGridField comparator = new ListGridField(gridAttributeName, listGridAttributeLabel);
		comparator.setWidth("*");
		comparator.setCanEdit(!Session.get().isDemo());
		comparator.setCellFormatter((value, rec, rowNum, colNum) -> {
			String label = getComparatorShortName(value != null ? value.toString() : null);
			boolean enabled = rec.getAttributeAsBoolean(EENABLED);
			if (!enabled)
				label = "<span style='color:red;'>" + label + "</span>";

			return label;
		});
		comparator.setFilterEditorProperties(ItemFactory.newFormatConverterSelector());

		associationsGrid.setFields(in, comparator);

		associationsGrid.setEditorCustomizer(context -> {
			ListGridField field = context.getEditField();

			if (field.getName().equals(gridAttributeName)) {
				final ListGridRecord selectedRecord = associationsGrid.getSelectedRecord();
				final SelectItem editorItem = ItemFactory
						.newComparatorSelector(selectedRecord.getAttributeAsString("in"));
				editorItem.setWidth("*");
				return editorItem;
			} else
				return context.getDefaultProperties();
		});

		associationsGrid.addEditCompleteHandler(event -> {
			Record converterRecord = settingsGrid.find(new AdvancedCriteria("id", OperatorId.EQUALS,
					associationsGrid.getSelectedRecord().getAttributeAsString(gridAttributeName)));
			if (converterRecord != null)
				associationsGrid.getSelectedRecord().setAttribute(EENABLED,
						converterRecord.getAttributeAsBoolean(EENABLED));
		});
	}

	protected void buildAssociationsGrid() {
		associationsGrid = new ListGrid();
		associationsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		associationsGrid.setShowFilterEditor(true);
		associationsGrid.setFilterOnKeypress(true);
		associationsGrid.setAutoFetchData(true);
		associationsGrid.setEditByCell(true);
		associationsGrid.setSelectionType(SelectionStyle.SINGLE);
		associationsGrid.setEditEvent(ListGridEditEvent.CLICK);
		associationsGrid.setDataSource(getAssociationsDataSource());
		associationsGrid.setAllowFilterOperators(true);
		associationsGrid.setShowRecordComponents(true);
		associationsGrid.setShowRecordComponentsByCell(true);
	}

	protected void prepareSettingsGrid() {
		settingsGrid = new ListGrid() {

			@Override
			protected Canvas getExpansionComponent(final ListGridRecord rec) {
				return buildSettingsGridExpansionComponent(rec);
			}
		};
		settingsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		settingsGrid.setSelectionType(SelectionStyle.SINGLE);
		settingsGrid.setCanExpandRecords(!Session.get().isDemo());
		settingsGrid.setShowFilterEditor(true);
		settingsGrid.setFilterOnKeypress(true);
		settingsGrid.setAutoFetchData(true);
		settingsGrid.setCanEdit(false);
		settingsGrid.setDataSource(getSettingsDataSource());

		ListGridField enabled = new ListGridField(EENABLED, " ", 20);
		enabled.setCanSort(false);
		enabled.setCanFilter(false);
		enabled.setCellFormatter((value, rec, rowNum, colNum) -> {
			if (value == null)
				return "";
			else if ("true".equals(value.toString()))
				return Util.imageHTML("bullet_green.png");
			else
				return Util.imageHTML("bullet_red.png");
		});

		ListGridField comparator = new ListGridField(gridAttributeName, listGridAttributeLabel);
		comparator.setWidth("*");
		comparator.setCellFormatter(
				(value, rec, rowNum, colNum) -> getComparatorShortName(value != null ? value.toString() : null));

		settingsGrid.setFields(enabled, comparator);
		settingsGrid.addCellContextClickHandler(event -> {
			event.cancel();
			showContextMenu();
		});
	}

	private Canvas buildSettingsGridExpansionComponent(final ListGridRecord rec) {
		VLayout layout = new VLayout(5);
		layout.setPadding(5);

		final ListGrid parametersGrid = new ListGrid();
		parametersGrid.setHeight(150);
		parametersGrid.setCanEdit(!Session.get().isDemo());
		parametersGrid.setModalEditing(true);
		parametersGrid.setAutoSaveEdits(true);
		parametersGrid.setAutoFetchData(true);

		ListGridField name = new ListGridField("name", I18N.message("parameter"), 150);
		name.setCanEdit(false);
		ListGridField value = new ListGridField(VALUE, I18N.message(VALUE));
		value.setWidth("*");
		value.setCanEdit(true);
		parametersGrid.setFields(name, value);

		parametersGrid.addCellSavedHandler(event -> {
			ListGridRecord paramRecord = event.getRecord();
			rec.setAttribute(paramRecord.getAttributeAsString("name"),
					event.getNewValue() != null ? event.getNewValue().toString() : "");
		});

		String[] attrs = rec.getAttributes();
		if (attrs != null && attrs.length > 0) {
			List<ListGridRecord> records = new ArrayList<>();
			for (String attr : attrs) {
				if (!isParameterAttribute(attr))
					continue;
				ListGridRecord recd = new ListGridRecord();
				recd.setAttribute("name", attr);
				recd.setAttribute(VALUE, rec.getAttributeAsString(attr));
				records.add(recd);
			}
			parametersGrid.setRecords(records.toArray(new ListGridRecord[0]));
		}

		// When the parameter's editing ends, update the same parameters
		// in all the records
		value.addEditorExitHandler(event -> {
			String parameterName = event.getRecord().getAttributeAsString("name");
			String parameterValue = event.getNewValue() != null ? event.getNewValue().toString() : "";
			rec.setAttribute(parameterName, parameterValue);
		});

		layout.addMember(parametersGrid);
		return layout;
	}

	protected DataSource getSettingsDataSource() {
		return new ComparatorsDS("-");
	}

	protected DataSource getAssociationsDataSource() {
		return new ComparatorsDS(null);
	}

	protected boolean isParameterAttribute(String name) {
		return !("id".equals(name) || gridAttributeName.equals(name) || "out".equals(name) || "in".equals(name)
				|| LABEL.equals(name) || EENABLED.equals(name) || name.startsWith("_"));
	}

	private void onSave() {
		List<GUIParameter> settings = collectSettings();

		SettingService.Instance.get().saveSettings(settings.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				GuiLog.info(I18N.message("settingssaved"), null);

				// Replicate the settings in the current session
				for (GUIParameter setting : settings) {
					Session.get().setConfig(setting.getName(), setting.getValue());
				}
			}
		});
	}

	private List<GUIParameter> collectSettings() {
		List<GUIParameter> settings = new ArrayList<>();

		for (Record rec : associationsGrid.getRecordList().toArray()) {
			String in = rec.getAttributeAsString("in").trim();
			String out = rec.getAttributeAsString("out") != null ? "-" + rec.getAttributeAsString("out").trim() : "";
			String comparator = rec.getAttributeAsString(gridAttributeName).trim();
			settings.add(new GUIParameter(settingsPrefix + in + out, comparator));
		}

		for (Record comparatorRecord : settingsGrid.getRecordList().toArray()) {
			collectComparatorAttributes(settings, comparatorRecord);
		}
		return settings;
	}

	private void collectComparatorAttributes(List<GUIParameter> settings, Record comparatorRecord) {
		Set<String> settingNames = new TreeSet<>();
		String comparator = comparatorRecord.getAttributeAsString(gridAttributeName).trim();
		String comparatorShort = getComparatorShortName(comparator);

		String[] attrs = comparatorRecord.getAttributes();
		if (attrs != null && attrs.length > 0) {
			for (String attr : attrs) {
				String attributeValue = comparatorRecord.getAttributeAsString(attr);
				if (isParameterAttribute(attr)) {
					String settingName = settingsPrefix + comparatorShort + "." + attr;
					if (settingNames.contains(settingName))
						continue;
					settingNames.add(settingName);
					settings.add(new GUIParameter(settingName, attributeValue));
				}
			}
		}
	}

	private String getComparatorShortName(String value) {
		if (value == null)
			return null;
		String str = value;
		if (str.contains("."))
			str = str.substring(str.lastIndexOf('.') + 1);
		return str;
	}

	protected void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem enable = prepareEnableMenuItem();

		if (Boolean.TRUE.equals(settingsGrid.getSelectedRecord().getAttributeAsBoolean(EENABLED)))
			enable.setEnabled(false);

		MenuItem disable = prepareDisableMenuItem();

		if (Boolean.FALSE.equals(settingsGrid.getSelectedRecord().getAttributeAsBoolean(EENABLED)))
			disable.setEnabled(false);

		contextMenu.setItems(enable, disable);
		contextMenu.showContextMenu();
	}

	private MenuItem prepareDisableMenuItem() {
		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> SettingService.Instance.get()
				.saveSettings(new GUIParameter[] { new GUIParameter(
						settingsPrefix + settingsGrid.getSelectedRecord().getAttribute(LABEL) + ".enabled", "false") },
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg0) {
								settingsGrid.getSelectedRecord().setAttribute(EENABLED, false);
								settingsGrid.refreshRow(settingsGrid.getRecordIndex(settingsGrid.getSelectedRecord()));

								// Update the associations table
								Record[] associations = associationsGrid.findAll(new AdvancedCriteria(gridAttributeName,
										OperatorId.EQUALS,
										settingsGrid.getSelectedRecord().getAttributeAsString(gridAttributeName)));
								if (associations != null)
									for (Record rec : associations)
										rec.setAttribute(EENABLED, false);

								// Refresh the visualization
								associationsGrid.invalidateRecordComponents();
								ListGridRecord[] recs = associationsGrid.getRecords();
								for (ListGridRecord rec : recs) {
									associationsGrid.refreshRecordComponent(associationsGrid.getRecordIndex(rec));
								}
								associationsGrid.refreshFields();
							}
						}));
		return disable;
	}

	private MenuItem prepareEnableMenuItem() {
		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(event -> SettingService.Instance.get()
				.saveSettings(new GUIParameter[] { new GUIParameter(
						settingsPrefix + settingsGrid.getSelectedRecord().getAttribute(LABEL) + ".enabled", "true") },
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg0) {
								settingsGrid.getSelectedRecord().setAttribute(EENABLED, true);
								settingsGrid.refreshRow(settingsGrid.getRecordIndex(settingsGrid.getSelectedRecord()));

								// Update the associations table
								Record[] associations = associationsGrid.findAll(new AdvancedCriteria(gridAttributeName,
										OperatorId.EQUALS,
										settingsGrid.getSelectedRecord().getAttributeAsString(gridAttributeName)));
								if (associations != null)
									for (Record rec : associations)
										rec.setAttribute(EENABLED, true);

								// Refresh the visualization
								associationsGrid.invalidateRecordComponents();
								ListGridRecord[] recs = associationsGrid.getRecords();
								for (ListGridRecord rec : recs) {
									associationsGrid.refreshRecordComponent(associationsGrid.getRecordIndex(rec));
								}
								associationsGrid.refreshFields();
							}
						}));
		return enable;
	}
}