package com.logicaldoc.gui.frontend.client.impex.converters;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.FormatConvertersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.settings.comparators.ComparatorAssociationsDialog;
import com.logicaldoc.gui.frontend.client.settings.comparators.ComparatorsPanel;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the format converters settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class FormatConvertersPanel extends ComparatorsPanel {

	private static final String EENABLED = "eenabled";

	public FormatConvertersPanel() {
		super("formatconverters");
		gridAttributeName = "converter";
		listGridAttributeLabel = I18N.message(gridAttributeName);
		settingsPrefix = gridAttributeName + ".";
		settingsGridTitle = I18N.message("converters");
	}

	@Override
	protected ToolStrip prepareToolStrip() {
		ToolStrip toolStrip = super.prepareToolStrip();

		ToolStripButton aliases = new ToolStripButton();
		aliases.setTitle(I18N.message("extensionaliases"));
		aliases.setDisabled(Session.get().isDemo());
		aliases.addClickHandler(event -> new ExtensionAliasesDialog().show());
		aliases.setDisabled(Session.get().isDemo());

		toolStrip.addSeparator();
		toolStrip.addButton(aliases);
		toolStrip.addFill();

		return toolStrip;
	}

	@Override
	protected void prepareAssociationsGrid() {
		buildAssociationsGrid();

		ListGridField in = new ListGridField("in", I18N.message("in"), 50);
		ListGridField converter = new ListGridField(gridAttributeName, listGridAttributeLabel);
		converter.setWidth("*");
		converter.setCanEdit(!Session.get().isDemo());
		converter.setCellFormatter((value, rec, rowNum, colNum) -> {
			String label = getConverterShortName(value != null ? value.toString() : null);
			boolean enabled = rec.getAttributeAsBoolean(EENABLED);
			if (!enabled)
				label = "<span style='color:red;'>" + label + "</span>";

			return label;
		});
		converter.setFilterEditorProperties(ItemFactory.newFormatConverterSelector());

		ListGridField out = new ListGridField("out", I18N.message("out"), 50);
		associationsGrid.setFields(in, out, converter);

		associationsGrid.setEditorCustomizer(context -> {
			ListGridField field = context.getEditField();

			if (field.getName().equals(gridAttributeName)) {
				final ListGridRecord selectedRecord = associationsGrid.getSelectedRecord();
				final SelectItem editorItem = ItemFactory.newFormatConverterSelector(
						selectedRecord.getAttributeAsString("in"), selectedRecord.getAttributeAsString("out"));
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

	@Override
	protected ComparatorAssociationsDialog getAssociationsDialog() {
		return new ConverterAssociationsDialog(associationsGrid);
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