package com.logicaldoc.gui.frontend.client.impex.converters;

import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.FormatConvertersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.settings.comparators.ComparatorsPanel;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the format converters settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class FormatConvertersPanel extends ComparatorsPanel {

	public FormatConvertersPanel() {
		super("formatconverters");
		recordAttributeForName = "converter";
		listGridAttributeLabel = I18N.message(recordAttributeForName);
		settingsPrefix = recordAttributeForName + ".";
		settingsGridTitle = I18N.message("converters");
	}

	@Override
	protected List<ToolStripButton> prepareConfigButtons() {
		List<ToolStripButton> buttons = super.prepareConfigButtons();

		ToolStripButton configButton = AwesomeFactory.newToolStripButton("gears", "associations");
		configButton.addClickHandler(click -> new ExtensionAliasesDialog().show());

		buttons.add(configButton);
		return buttons;
	}

	@Override
	protected void prepareAssociationsGrid() {
		buildAssociationsGrid();

		ListGridField in = new ListGridField("in", I18N.message("in"), 60);
		ListGridField converter = new ListGridField(recordAttributeForName, listGridAttributeLabel);
		converter.setWidth("*");
		converter.setCanEdit(!Session.get().isDemo());
		converter.setCellFormatter((value, rec, rowNum, colNum) -> {
			String label = getConverterShortName(value != null ? value.toString() : null);
			if (Boolean.FALSE.equals(rec.getAttributeAsBoolean(ENABLED)))
				label = "<span style='color:red;'>" + label + "</span>";

			return label;
		});
		converter.setFilterEditorProperties(ItemFactory.newFormatConverterSelector());

		ListGridField out = new ListGridField("out", I18N.message("out"), 60);
		associationsGrid.setFields(in, out, converter);

		associationsGrid.setEditorCustomizer(context -> {
			ListGridField field = context.getEditField();

			if (field.getName().equals(recordAttributeForName)) {
				final ListGridRecord selectedRecord = associationsGrid.getSelectedRecord();
				final SelectItem editorItem = ItemFactory.newFormatConverterSelector(
						selectedRecord.getAttributeAsString("in"), selectedRecord.getAttributeAsString("out"));
				editorItem.setWidth("*");
				return editorItem;
			} else
				return context.getDefaultProperties();
		});
	}

	@Override
	protected DataSource getSettingsDataSource() {
		return new FormatConvertersDS("-", "-");
	}

	@Override
	protected DataSource getAssociationsDataSource() {
		return new FormatConvertersDS(null, null);
	}

	private String getConverterShortName(String value) {
		if (value == null)
			return null;
		String str = value;
		if (str.contains("."))
			str = str.substring(str.lastIndexOf('.') + 1);
		return str;
	}
}