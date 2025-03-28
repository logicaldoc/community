package com.logicaldoc.gui.frontend.client.metadata.template;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A window to select extended attributes.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class AttributeSelector extends Window {

	private static final String PRESET = "preset";

	private static final String LABEL = "label";

	private ListGrid setAttributesList;

	private AttributeSelectorCallback callback;

	public AttributeSelector(AttributeSelectorCallback callback) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("attributesselector"));
		setWidth(500);
		setHeight(400);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		centerInPage();
		setAutoSize(true);

		addCloseClickHandler(event -> destroy());

		this.callback = callback;
	}

	@Override
	public void onDraw() {
		setAttributesList = new ListGrid();
		setAttributesList.setEmptyMessage(I18N.message("notitemstoshow"));
		setAttributesList.setWidth100();
		setAttributesList.setHeight(getHeight());
		setAttributesList.setEmptyMessage(I18N.message("norecords"));
		setAttributesList.setCanReorderRecords(false);
		setAttributesList.setCanSort(false);
		setAttributesList.setCanFreezeFields(false);
		setAttributesList.setCanGroupBy(false);
		setAttributesList.setLeaveScrollbarGap(false);
		setAttributesList.setShowHeader(true);
		setAttributesList.setSelectionType(SelectionStyle.MULTIPLE);
		setAttributesList.setCanEdit(false);
		setAttributesList.setShowRowNumbers(true);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setCanEdit(false);
		name.setCanSort(false);
		name.setAutoFitWidth(true);
		name.setMinWidth(80);

		ListGridField label = new ListGridField(LABEL, I18N.message(LABEL));
		label.setCanEdit(true);
		label.setCanSort(false);
		label.setAutoFitWidth(true);
		label.setMinWidth(80);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setCanEdit(false);
		type.setCanSort(false);
		type.setAutoFitWidth(true);
		type.setMinWidth(70);
		type.setCellFormatter(new AttributeTypeFormatter());

		ListGridField preset = new ListGridField(PRESET, I18N.message(PRESET));
		preset.setCanEdit(false);
		preset.setCanSort(false);
		preset.setAutoFitWidth(true);
		preset.setMinWidth(70);

		setAttributesList.setFields(name, label, type, preset);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		SelectItem setSelector = ItemFactory.newAttributeSetSelector();
		toolStrip.addFormItem(setSelector);
		setSelector.addChangedHandler(event -> {
			if (event.getValue() == null)
				fillSetAttributesList(null);
			else
				fillSetAttributesList(Long.parseLong(event.getValue().toString()));
		});

		toolStrip.addSeparator();

		ToolStripButton select = new ToolStripButton();
		select.setTitle(I18N.message("select"));
		toolStrip.addButton(select);
		select.addClickHandler(event -> {
			if (setAttributesList.getSelectedRecords() == null || setAttributesList.getSelectedRecords().length < 1)
				SC.warn(I18N.message("pleaseselectanattribute"));
			else
				callback.onSelection(setAttributesList.getSelectedRecords());
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		toolStrip.addButton(close);
		close.addClickHandler(event -> destroy());
		toolStrip.addFill();

		addItem(toolStrip);
		addItem(setAttributesList);
	}

	protected void fillSetAttributesList(Long setId) {
		AttributeSetService.Instance.get().getAttributeSet(setId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIAttributeSet set) {
				ListGridRecord[] records = setAttributesList.getRecords();
				if (records != null)
					for (ListGridRecord rec : records)
						setAttributesList.removeData(rec);

				for (GUIAttribute att : set.getAttributes()) {
					ListGridRecord rec = new ListGridRecord();
					rec.setAttribute("name", att.getName());
					rec.setAttribute(LABEL, att.getLabel());
					rec.setAttribute("set", att.getSet());
					rec.setAttribute("setId", att.getSetId());
					rec.setAttribute("type", att.getType());
					rec.setAttribute("editor", att.getEditor());
					rec.setAttribute("mandatory", att.isMandatory());
					rec.setAttribute("hidden", att.isHidden());
					rec.setAttribute("readonly", att.isReadonly());
					rec.setAttribute("multiple", att.isMultiple());
					rec.setAttribute("validation", att.getValidation());
					rec.setAttribute("initialization", att.getInitialization());
					rec.setAttribute(PRESET, att.getEditor() == GUIAttribute.EDITOR_LISTBOX);
					setAttributesList.getRecordList().add(rec);
				}
			}
		});
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	public interface AttributeSelectorCallback {
		public void onSelection(ListGridRecord[] selection);
	}
}