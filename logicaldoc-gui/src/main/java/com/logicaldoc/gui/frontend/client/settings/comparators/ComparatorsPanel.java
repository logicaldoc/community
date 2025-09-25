package com.logicaldoc.gui.frontend.client.settings.comparators;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.ComparatorsDS;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
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
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the comparators settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class ComparatorsPanel extends AdminPanel {

	protected static final String LABEL = "label";

	protected static final String VALUE = "value";

	protected static final String ENABLED = "eenabled";

	protected String recordAttributeForName = "comparator";

	protected String settingsPrefix = recordAttributeForName + ".";

	protected String listGridAttributeLabel = I18N.message(recordAttributeForName);

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

	protected List<ToolStripButton> prepareConfigButtons() {
		return new ArrayList<>();
	}

	@Override
	protected void onDraw() {
		prepareAssociationsGrid();

		SectionStackSection associationsSection = new SectionStackSection(I18N.message("associations"));
		associationsSection.setCanCollapse(false);
		associationsSection.setExpanded(true);
		associationsSection.setItems(associationsGrid);

		SectionStack associationsStack = new SectionStack();
		associationsStack.setWidth(350);
		associationsStack.setHeight100();
		associationsStack.setSections(associationsSection);

		List<ToolStripButton> configButtons = prepareConfigButtons();
		if (!configButtons.isEmpty() && !Session.get().isDemo())
			associationsSection.setControls(configButtons.toArray(new Canvas[0]));

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
	
	protected void prepareAssociationsGrid() {
		buildAssociationsGrid();

		ListGridField in = new ListGridField("in", I18N.message("ext"), 60);
		ListGridField comparator = new ListGridField(recordAttributeForName, listGridAttributeLabel);
		comparator.setWidth("*");
		comparator.setCanEdit(!Session.get().isDemo());
		comparator.setCellFormatter((value, rec, rowNum, colNum) -> {
			String label = getComparatorShortName(value != null ? value.toString() : null);
			boolean enabled = rec.getAttributeAsBoolean(ENABLED);
			if (!enabled)
				label = "<span style='color:red;'>" + label + "</span>";

			return label;
		});
		comparator.setFilterEditorProperties(ItemFactory.newFormatConverterSelector());

		associationsGrid.setFields(in, comparator);

		associationsGrid.setEditorCustomizer(context -> {
			ListGridField field = context.getEditField();

			if (field.getName().equals(recordAttributeForName)) {
				final ListGridRecord selectedRecord = associationsGrid.getSelectedRecord();
				final SelectItem editorItem = ItemFactory
						.newComparatorSelector(selectedRecord.getAttributeAsString("in"));
				editorItem.setWidth("*");
				return editorItem;
			} else
				return context.getDefaultProperties();
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

		associationsGrid.addCellSavedHandler(saved -> {
			Record converterRecord = settingsGrid.find(new AdvancedCriteria("id", OperatorId.EQUALS,
					associationsGrid.getSelectedRecord().getAttributeAsString(recordAttributeForName)));
			if (converterRecord != null)
				associationsGrid.getSelectedRecord().setAttribute(ENABLED,
						Boolean.TRUE.equals(converterRecord.getAttributeAsBoolean(ENABLED)));

			saveAssociations(saved.getRecord());
		});
	}

	public void saveAssociations(ListGridRecord... records) {
		List<GUIParameter> parameters = new ArrayList<>();
		for (ListGridRecord associationRecord : records) {
			Record converterRecord = settingsGrid.find(new AdvancedCriteria("id", OperatorId.EQUALS,
					associationsGrid.getSelectedRecord().getAttributeAsString(recordAttributeForName)));
			if (converterRecord != null)
				associationsGrid.getSelectedRecord().setAttribute(ENABLED,
						Boolean.TRUE.equals(converterRecord.getAttributeAsBoolean(ENABLED)));

			String input = associationRecord.getAttributeAsString("in").trim();
			String comparatorName = associationRecord.getAttributeAsString(recordAttributeForName).trim();

			// Out is only for format converters
			String output = associationRecord.getAttributeAsString("out") != null
					? "-" + associationRecord.getAttributeAsString("out").trim()
					: "";

			parameters.add(new GUIParameter(settingsPrefix + input + output, comparatorName));
		}

		SettingService.Instance.get().saveSettings(parameters, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void arg) {
				super.onSuccess(arg);

				GuiLog.info(I18N.message("settingssaved"), null);

				// Replicate the setting in the current session
				for (GUIParameter setting : parameters)
					Session.get().setConfig(setting.getName(), setting.getValue());
			}
		});
	}

	protected void prepareSettingsGrid() {
		settingsGrid = new ListGrid() {

			@Override
			protected Canvas getExpansionComponent(final ListGridRecord rec) {
				return buildSettingsGrid(rec);
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

		ListGridField enabled = new EnabledListGridField();

		ListGridField comparator = new ListGridField(recordAttributeForName, listGridAttributeLabel);
		comparator.setWidth("*");
		comparator.setCellFormatter(
				(value, rec, rowNum, colNum) -> getComparatorShortName(value != null ? value.toString() : null));

		settingsGrid.setFields(enabled, comparator);
		settingsGrid.addCellContextClickHandler(event -> {
			event.cancel();
			showContextMenu();
		});
	}

	/**
	 * Builds the grid where the user can see and edit the settings of an
	 * element
	 * 
	 * @param element the element in editing
	 * 
	 * @return The editable grid with all the settings for the given element
	 */
	private Canvas buildSettingsGrid(final ListGridRecord element) {
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

		parametersGrid.addCellSavedHandler(saved -> {
			ListGridRecord paramRecord = saved.getRecord();
			String paramName = paramRecord.getAttributeAsString("name");
			String newValue = saved.getNewValue() != null ? saved.getNewValue().toString() : "";

			element.setAttribute(paramName, newValue);

			String comparator = element.getAttributeAsString(recordAttributeForName).trim();
			String comparatorShort = getComparatorShortName(comparator);

			GUIParameter setting = new GUIParameter(settingsPrefix + comparatorShort + "." + paramName, newValue);
			SettingService.Instance.get().saveSettings(Arrays.asList(setting), new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Void arg) {
					super.onSuccess(arg);

					GuiLog.info(I18N.message("settingssaved"), null);

					// Replicate the setting in the current session
					Session.get().setConfig(setting.getName(), setting.getValue());
				}
			});
		});

		String[] attrs = element.getAttributes();
		if (attrs != null && attrs.length > 0) {
			List<ListGridRecord> records = new ArrayList<>();
			for (String attr : attrs) {
				if (!isParameterAttribute(attr))
					continue;
				ListGridRecord recd = new ListGridRecord();
				recd.setAttribute("name", attr);
				recd.setAttribute(VALUE, element.getAttributeAsString(attr));
				records.add(recd);
			}
			parametersGrid.setRecords(records.toArray(new ListGridRecord[0]));
		}

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
		return !("id".equals(name) || recordAttributeForName.equals(name) || "out".equals(name) || "in".equals(name)
				|| LABEL.equals(name) || ENABLED.equals(name) || name.startsWith("_"));
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
		enable.setEnabled(Boolean.FALSE.equals(settingsGrid.getSelectedRecord().getAttributeAsBoolean(ENABLED)));

		MenuItem disable = prepareDisableMenuItem();
		disable.setEnabled(Boolean.TRUE.equals(settingsGrid.getSelectedRecord().getAttributeAsBoolean(ENABLED)));

		contextMenu.setItems(enable, disable);
		contextMenu.showContextMenu();
	}

	private MenuItem prepareDisableMenuItem() {
		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(event -> SettingService.Instance.get()
				.saveSettings(Arrays.asList(new GUIParameter(
						settingsPrefix + settingsGrid.getSelectedRecord().getAttribute(LABEL) + ".enabled", "false")),
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void arg0) {
								settingsGrid.getSelectedRecord().setAttribute(ENABLED, false);
								settingsGrid.refreshRow(settingsGrid.getRecordIndex(settingsGrid.getSelectedRecord()));

								// Update the associations table
								Record[] associations = associationsGrid.findAll(new AdvancedCriteria(
										recordAttributeForName, OperatorId.EQUALS,
										settingsGrid.getSelectedRecord().getAttributeAsString(recordAttributeForName)));
								if (associations != null)
									for (Record rec : associations)
										rec.setAttribute(ENABLED, false);

								// Refresh the visualization
								associationsGrid.invalidateRecordComponents();
								ListGridRecord[] recs = associationsGrid.getRecords();
								for (ListGridRecord rec : recs)
									associationsGrid.refreshRecordComponent(associationsGrid.getRecordIndex(rec));
								associationsGrid.refreshFields();
							}
						}));
		return disable;
	}

	private MenuItem prepareEnableMenuItem() {
		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(event -> SettingService.Instance.get()
				.saveSettings(Arrays.asList(new GUIParameter(
						settingsPrefix + settingsGrid.getSelectedRecord().getAttribute(LABEL) + ".enabled", "true")),
						new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void arg0) {
								settingsGrid.getSelectedRecord().setAttribute(ENABLED, true);
								settingsGrid.refreshRow(settingsGrid.getRecordIndex(settingsGrid.getSelectedRecord()));

								// Update the associations table
								Record[] associations = associationsGrid.findAll(new AdvancedCriteria(
										recordAttributeForName, OperatorId.EQUALS,
										settingsGrid.getSelectedRecord().getAttributeAsString(recordAttributeForName)));
								if (associations != null)
									for (Record rec : associations)
										rec.setAttribute(ENABLED, true);

								// Refresh the visualization
								associationsGrid.invalidateRecordComponents();
								ListGridRecord[] recs = associationsGrid.getRecords();
								for (ListGridRecord rec : recs)
									associationsGrid.refreshRecordComponent(associationsGrid.getRecordIndex(rec));
								associationsGrid.refreshFields();
							}
						}));
		return enable;
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