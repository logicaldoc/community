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
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridEditorContext;
import com.smartgwt.client.widgets.grid.ListGridEditorCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
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
		aliases.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ExtensionAliasesDialog dialog = new ExtensionAliasesDialog();
				dialog.show();
			}
		});
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

		ListGridField out = new ListGridField("out", I18N.message("out"), 50);
		associationsGrid.setFields(in, out, converter);

		associationsGrid.setEditorCustomizer(new ListGridEditorCustomizer() {
			public FormItem getEditor(ListGridEditorContext context) {
				ListGridField field = context.getEditField();

				if (field.getName().equals(gridAttributeName)) {
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
						associationsGrid.getSelectedRecord().getAttributeAsString(gridAttributeName)));
				if (converterRecord != null)
					associationsGrid.getSelectedRecord().setAttribute("eenabled",
							converterRecord.getAttributeAsBoolean("eenabled"));
			}
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

	private String getConverterShortName(String value) {
		if (value == null)
			return null;
		String str = value;
		if (str.contains("."))
			str = str.substring(str.lastIndexOf('.') + 1);
		return str;
	}
}