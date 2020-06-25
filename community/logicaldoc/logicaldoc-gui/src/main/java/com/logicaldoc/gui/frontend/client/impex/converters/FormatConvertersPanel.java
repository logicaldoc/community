package com.logicaldoc.gui.frontend.client.impex.converters;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.FormatConvertersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridEditorContext;
import com.smartgwt.client.widgets.grid.ListGridEditorCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellSavedEvent;
import com.smartgwt.client.widgets.grid.events.CellSavedHandler;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;
import com.smartgwt.client.widgets.grid.events.EditorExitEvent;
import com.smartgwt.client.widgets.grid.events.EditorExitHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the format converters settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class FormatConvertersPanel extends AdminPanel {

	private static final String SETTING_PREFIX = "converter.";

	// Associations between file formats and a converter
	private ListGrid associationsGrid;

	// Settings for the different converters
	private ListGrid settingsGrid;

	public FormatConvertersPanel() {
		super("formatconverters");
		initGUI();
	}

	private void initGUI() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.setDisabled(Session.get().isDemo());
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		save.setDisabled(Session.get().isDemo());

		ToolStripButton aliases = new ToolStripButton();
		aliases.setTitle(I18N.message("extensionaliases"));
		aliases.setDisabled(Session.get().isDemo());
		aliases.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ExtensionAliasesDialog dialog = new ExtensionAliasesDialog();
				dialog.show();
			}
		});
		save.setDisabled(Session.get().isDemo());

		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(aliases);
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
		configButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				ConverterAssociationsDialog dialog = new ConverterAssociationsDialog(associationsGrid);
				dialog.show();
			}
		});
		if (!Session.get().isDemo())
			associationsSection.setControls(configButton);

		SectionStack associationsStack = new SectionStack();
		associationsStack.setWidth(350);
		associationsStack.setHeight100();
		associationsStack.setSections(associationsSection);

		prepareSettingsGrid();
		SectionStackSection settingsSection = new SectionStackSection(I18N.message("converters"));
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

	private void prepareAssociationsGrid() {
		associationsGrid = new ListGrid();
		associationsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		associationsGrid.setShowFilterEditor(true);
		associationsGrid.setFilterOnKeypress(true);
		associationsGrid.setAutoFetchData(true);
		associationsGrid.setEditByCell(true);
		associationsGrid.setSelectionType(SelectionStyle.SINGLE);
		associationsGrid.setEditEvent(ListGridEditEvent.CLICK);
		associationsGrid.setDataSource(new FormatConvertersDS(null, null));
		associationsGrid.setAllowFilterOperators(true);
		associationsGrid.setShowRecordComponents(true);
		associationsGrid.setShowRecordComponentsByCell(true);

		ListGridField in = new ListGridField("in", I18N.message("in"), 40);
		ListGridField out = new ListGridField("out", I18N.message("out"), 40);
		ListGridField converter = new ListGridField("converter", I18N.message("converter"));
		converter.setWidth("*");
		converter.setCanEdit(!Session.get().isDemo());
		converter.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				String label = getConverterShortName(value != null ? value.toString() : null);
				boolean enabled = record.getAttributeAsBoolean("eenabled");
				if (!enabled)
					label = "<span style='color:red;'>" + label + "</span>";

				return label;
			}
		});
		converter.setFilterEditorProperties(ItemFactory.newFormatConverterSelector());

		associationsGrid.setFields(in, out, converter);

		associationsGrid.setEditorCustomizer(new ListGridEditorCustomizer() {
			public FormItem getEditor(ListGridEditorContext context) {
				ListGridField field = context.getEditField();

				if (field.getName().equals("converter")) {
					final ListGridRecord selectedRecord = associationsGrid.getSelectedRecord();
					final SelectItem editorItem = ItemFactory.newFormatConverterSelector(
							selectedRecord.getAttributeAsString("in"), selectedRecord.getAttributeAsString("out"));
					editorItem.setWidth("*");
					return editorItem;
				} else
					return context.getDefaultProperties();
			}
		});

		associationsGrid.addEditCompleteHandler(new EditCompleteHandler() {

			@Override
			public void onEditComplete(EditCompleteEvent event) {
				Record converterRecord = settingsGrid.find(new AdvancedCriteria("id", OperatorId.EQUALS,
						associationsGrid.getSelectedRecord().getAttributeAsString("converter")));
				if (converterRecord != null)
					associationsGrid.getSelectedRecord().setAttribute("eenabled",
							converterRecord.getAttributeAsBoolean("eenabled"));
			}
		});
	}

	private void prepareSettingsGrid() {
		settingsGrid = new ListGrid() {

			@Override
			protected Canvas getExpansionComponent(final ListGridRecord record) {
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
				ListGridField value = new ListGridField("value", I18N.message("value"));
				value.setWidth("*");
				value.setCanEdit(true);
				parametersGrid.setFields(name, value);

				parametersGrid.addCellSavedHandler(new CellSavedHandler() {
					@Override
					public void onCellSaved(CellSavedEvent event) {
						ListGridRecord paramRecord = event.getRecord();
						record.setAttribute(paramRecord.getAttributeAsString("name"),
								event.getNewValue() != null ? event.getNewValue().toString() : "");
					}
				});

				String[] attrs = record.getAttributes();
				if (attrs != null && attrs.length > 0) {
					List<ListGridRecord> records = new ArrayList<ListGridRecord>();
					for (String attr : attrs) {
						if (!isParameterAttribute(attr))
							continue;
						ListGridRecord rec = new ListGridRecord();
						rec.setAttribute("name", attr);
						rec.setAttribute("value", record.getAttributeAsString(attr));
						records.add(rec);
					}
					parametersGrid.setRecords(records.toArray(new ListGridRecord[0]));
				}

				// When the parameter's editing ends, update the same parameters
				// in all the records
				value.addEditorExitHandler(new EditorExitHandler() {

					@Override
					public void onEditorExit(EditorExitEvent event) {
						String parameterName = event.getRecord().getAttributeAsString("name");
						String parameterValue = event.getNewValue() != null ? event.getNewValue().toString() : "";
						record.setAttribute(parameterName, parameterValue);
					}
				});

				layout.addMember(parametersGrid);
				return layout;
			}
		};
		settingsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		settingsGrid.setSelectionType(SelectionStyle.SINGLE);
		settingsGrid.setCanExpandRecords(!Session.get().isDemo());
		settingsGrid.setShowFilterEditor(true);
		settingsGrid.setFilterOnKeypress(true);
		settingsGrid.setAutoFetchData(true);
		settingsGrid.setCanEdit(false);
		settingsGrid.setDataSource(new FormatConvertersDS("-", "-"));

		ListGridField enabled = new ListGridField("eenabled", " ", 20);
		enabled.setCanSort(false);
		enabled.setCanFilter(false);
		enabled.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if (value == null)
					return "";
				else if ("true".equals(value.toString()))
					return Util.imageHTML("bullet_green.png");
				else
					return Util.imageHTML("bullet_red.png");
			}
		});

		ListGridField converter = new ListGridField("converter", I18N.message("converter"));
		converter.setWidth("*");
		converter.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return getConverterShortName(value != null ? value.toString() : null);
			}
		});

		settingsGrid.setFields(enabled, converter);
		settingsGrid.addCellContextClickHandler(new CellContextClickHandler() {

			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				event.cancel();
				showContextMenu();
			}
		});
	}

	private static boolean isParameterAttribute(String name) {
		if ("id".equals(name) || "converter".equals(name) || "in".equals(name) || "out".equals(name)
				|| "label".equals(name) || "eenabled".equals(name) || name.startsWith("_"))
			return false;
		else
			return true;
	}

	private void onSave() {
		final List<GUIParameter> settings = new ArrayList<GUIParameter>();

		final Set<String> settingNames = new TreeSet<String>();

		for (Record rec : associationsGrid.getRecordList().toArray()) {
			String id = rec.getAttributeAsString("id").trim();
			String converter = rec.getAttributeAsString("converter").trim();
			settings.add(new GUIParameter(SETTING_PREFIX + id, converter));
		}

		for (Record rec : settingsGrid.getRecordList().toArray()) {
			String converter = rec.getAttributeAsString("converter").trim();
			String converterShort = getConverterShortName(converter);

			String[] attrs = rec.getAttributes();
			if (attrs != null && attrs.length > 0) {
				for (String attr : attrs) {
					String attributeValue = rec.getAttributeAsString(attr);
					if (!isParameterAttribute(attr))
						continue;
					String settingName = SETTING_PREFIX + converterShort + "." + attr;
					if (settingNames.contains(settingName))
						continue;
					settingNames.add(settingName);
					settings.add(new GUIParameter(settingName, attributeValue));
				}
			}
		}

		SettingService.Instance.get().saveSettings(settings.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				Log.info(I18N.message("settingssaved"), null);

				// Replicate the settings in the current session
				for (GUIParameter setting : settings)
					Session.get().setConfig(setting.getName(), setting.getValue());
			}
		});
	}

	private String getConverterShortName(String value) {
		if (value == null)
			return null;
		String str = value;
		if (str.contains("."))
			str = str.substring(str.lastIndexOf('.') + 1);
		return str;
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem enable = new MenuItem();
		enable.setTitle(I18N.message("enable"));
		enable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SettingService.Instance.get()
						.saveSettings(new GUIParameter[] { new GUIParameter(
								SETTING_PREFIX + settingsGrid.getSelectedRecord().getAttribute("label") + ".enabled",
								"true") }, new AsyncCallback<Void>() {

									@Override
									public void onFailure(Throwable caught) {
										Log.serverError(caught);
									}

									@Override
									public void onSuccess(Void arg0) {
										settingsGrid.getSelectedRecord().setAttribute("eenabled", true);
										settingsGrid.refreshRow(
												settingsGrid.getRecordIndex(settingsGrid.getSelectedRecord()));

										// Update the associations table
										Record[] associations = associationsGrid.findAll(new AdvancedCriteria(
												"converter", OperatorId.EQUALS,
												settingsGrid.getSelectedRecord().getAttributeAsString("converter")));
										if (associations != null)
											for (Record record : associations)
												record.setAttribute("eenabled", true);

										// Refresh the visualization
										associationsGrid.invalidateRecordComponents();
										ListGridRecord[] recs = associationsGrid.getRecords();
										for (ListGridRecord rec : recs) {
											associationsGrid
													.refreshRecordComponent(associationsGrid.getRecordIndex(rec));
										}
										associationsGrid.refreshFields();
									}
								});
			}
		});

		if (settingsGrid.getSelectedRecord().getAttributeAsBoolean("eenabled"))
			enable.setEnabled(false);

		MenuItem disable = new MenuItem();
		disable.setTitle(I18N.message("disable"));
		disable.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SettingService.Instance.get()
						.saveSettings(new GUIParameter[] { new GUIParameter(
								SETTING_PREFIX + settingsGrid.getSelectedRecord().getAttribute("label") + ".enabled",
								"false") }, new AsyncCallback<Void>() {

									@Override
									public void onFailure(Throwable caught) {
										Log.serverError(caught);
									}

									@Override
									public void onSuccess(Void arg0) {
										settingsGrid.getSelectedRecord().setAttribute("eenabled", false);
										settingsGrid.refreshRow(
												settingsGrid.getRecordIndex(settingsGrid.getSelectedRecord()));

										// Update the associations table
										Record[] associations = associationsGrid.findAll(new AdvancedCriteria(
												"converter", OperatorId.EQUALS,
												settingsGrid.getSelectedRecord().getAttributeAsString("converter")));
										if (associations != null)
											for (Record record : associations)
												record.setAttribute("eenabled", false);

										// Refresh the visualization
										associationsGrid.invalidateRecordComponents();
										ListGridRecord[] recs = associationsGrid.getRecords();
										for (ListGridRecord rec : recs) {
											associationsGrid
													.refreshRecordComponent(associationsGrid.getRecordIndex(rec));
										}
										associationsGrid.refreshFields();
									}
								});
			}
		});

		if (!settingsGrid.getSelectedRecord().getAttributeAsBoolean("eenabled"))
			disable.setEnabled(false);

		contextMenu.setItems(enable, disable);
		contextMenu.showContextMenu();
	}
}