package com.logicaldoc.gui.frontend.client.metadata.template;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class AddTemplateAttributeDialog extends Window {

	private ListGrid setAttributesList;

	private TemplatePropertiesPanel propertiesPanel;

	public AddTemplateAttributeDialog(TemplatePropertiesPanel panel) {
		this.propertiesPanel = panel;

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("attributesets"));
		setWidth(500);
		setHeight(400);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		centerInPage();
		setAutoSize(true);

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
		name.setWidth(100);

		ListGridField label = new ListGridField("label", I18N.message("label"));
		label.setCanEdit(true);
		label.setCanSort(false);
		label.setWidth(150);

		ListGridField type = new ListGridField("type", I18N.message("type"));
		type.setCanEdit(false);
		type.setCanSort(false);
		type.setWidth(100);
		type.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if (value == null)
					return "";
				int intValue = new Integer(value.toString());
				switch (intValue) {
				case GUIAttribute.TYPE_STRING:
					return I18N.message("string");
				case GUIAttribute.TYPE_INT:
					return I18N.message("integer");
				case GUIAttribute.TYPE_DOUBLE:
					return I18N.message("decimal");
				case GUIAttribute.TYPE_DATE:
					return I18N.message("date");
				case GUIAttribute.TYPE_BOOLEAN:
					return I18N.message("boolean");
				case GUIAttribute.TYPE_USER:
					return I18N.message("user");
				}
				return value.toString();
			}
		});

		setAttributesList.setFields(name, label, type);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		SelectItem setSelector = ItemFactory.newAttributeSetSelector();
		toolStrip.addFormItem(setSelector);
		setSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				if (event.getValue() == null)
					fillSetAttributesList(null);
				else
					fillSetAttributesList(new Long(event.getValue().toString()));
			}
		});

		toolStrip.addSeparator();

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addselection"));
		toolStrip.addButton(add);
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (setAttributesList.getSelectedRecords() == null || setAttributesList.getSelectedRecords().length < 1)
					SC.warn(I18N.message("pleaseselectanattribute"));
				else
					propertiesPanel.addAttributes(setAttributesList.getSelectedRecords());
			}
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		toolStrip.addButton(close);
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				destroy();
			}
		});
		toolStrip.addFill();

		addItem(toolStrip);
		addItem(setAttributesList);
	}

	protected void fillSetAttributesList(Long setId) {
		AttributeSetService.Instance.get().getAttributeSet(setId, new AsyncCallback<GUIAttributeSet>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAttributeSet set) {
				ListGridRecord[] records = setAttributesList.getRecords();
				if (records != null)
					for (ListGridRecord record : records) {
						setAttributesList.removeData(record);
					}

				GUIAttribute[] attributes = set.getAttributes();

				if (attributes == null)
					return;

				for (int i = 0; i < attributes.length; i++) {
					GUIAttribute att = attributes[i];
					ListGridRecord record = new ListGridRecord();
					record.setAttribute("name", att.getName());
					record.setAttribute("label", att.getLabel());
					record.setAttribute("set", att.getSet());
					record.setAttribute("setId", att.getSetId());
					record.setAttribute("type", att.getType());
					record.setAttribute("editor", att.getEditor());
					record.setAttribute("mandatory", att.isMandatory());
					setAttributesList.getRecordList().add(record);
				}
			}
		});
	}
}
