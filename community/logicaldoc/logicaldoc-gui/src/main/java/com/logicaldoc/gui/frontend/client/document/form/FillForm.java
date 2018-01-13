package com.logicaldoc.gui.frontend.client.document.form;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This popup window is used get the form's fields values
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FillForm extends Window {
	private DynamicForm form = new DynamicForm();

	private GUIDocument frm;

	public FillForm(GUIDocument frm) {
		this.frm = frm;

		VLayout layout = new VLayout();
		layout.setMargin(5);
		layout.setMembersMargin(3);

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("form") + " - " + Util.getBaseName(frm.getFileName()));
		setWidth(500);
		setHeight(400);
		// setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		List<FormItem> items = new ArrayList<FormItem>();
		for (GUIAttribute att : frm.getAttributes()) {
			if (att.getType() == GUIAttribute.TYPE_STRING) {
				FormItem item = ItemFactory.newStringItemForAttribute(att);
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_INT) {
				FormItem item = ItemFactory.newIntegerItemForAttribute(att.getName(), att.getLabel(), null);
				item.setRequired(att.isMandatory());
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
				SelectItem item = ItemFactory.newBooleanSelectorForAttribute(att.getName(), att.getLabel(),
						!att.isMandatory());
				item.setRequired(att.isMandatory());
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
				FormItem item = ItemFactory.newFloatItemForAttribute(att.getName(), att.getLabel(), null);
				item.setRequired(att.isMandatory());
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_DATE) {
				final DateItem item = ItemFactory.newDateItemForAttribute(att.getName(), att.getLabel());
				item.setRequired(att.isMandatory());
				item.addKeyPressHandler(new KeyPressHandler() {
					@Override
					public void onKeyPress(KeyPressEvent event) {
						if ("backspace".equals(event.getKeyName().toLowerCase())
								|| "delete".equals(event.getKeyName().toLowerCase())) {
							item.clearValue();
							item.setValue((Date) null);
						}
					}
				});
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_USER) {
				SelectItem item = ItemFactory.newUserSelectorForAttribute(att.getName(), att.getLabel(),
						(att.getOptions() != null && att.getOptions().length > 0) ? att.getOptions()[0] : null);
				item.setRequired(att.isMandatory());
				items.add(item);
			}
		}

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});

		HLayout buttonsBar = new HLayout();
		buttonsBar.setWidth100();
		buttonsBar.setHeight(25);
		buttonsBar.setMembers(save);

		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setItems(items.toArray(new FormItem[0]));

		Tab tab = new Tab(I18N.message("formfields"));
		tab.setPane(form);

		TabSet tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();
		tabSet.addTab(tab);

		layout.setMembers(tabSet, buttonsBar);

		addItem(layout);
	}

	public void onSave() {
		if (!form.validate())
			return;

		Map<String, Object> values = (Map<String, Object>) form.getValues();
		for (String name : values.keySet()) {
			if (name.startsWith("_")) {
				Object val = values.get(name);
				String nm = name.substring(1).replaceAll(Constants.BLANK_PLACEHOLDER, " ");
				GUIAttribute att = frm.getAttribute(nm);
				if (att == null)
					continue;

				if (val != null) {
					if (att.getType() == GUIAttribute.TYPE_USER) {
						SelectItem userItem = (SelectItem) form.getItem(name);
						if (userItem.getValue() != null && !"".equals(userItem.getValue())) {
							ListGridRecord sel = userItem.getSelectedRecord();

							// Prepare a dummy user to set as
							// attribute
							// value
							GUIUser dummy = new GUIUser();
							dummy.setId(Long.parseLong(val.toString()));
							dummy.setFirstName(sel.getAttributeAsString("firstName"));
							dummy.setName(sel.getAttributeAsString("name"));
							frm.setValue(nm, dummy);
						} else {
							GUIAttribute at = frm.getAttribute(nm);
							at.setIntValue(null);
							at.setStringValue(null);
							at.setType(GUIAttribute.TYPE_USER);
						}
					} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
						if (!(val == null || "".equals(val.toString().trim())))
							frm.setValue(nm, "1".equals(val.toString().trim()) ? true : false);
						else if (frm.getAttribute(nm) != null) {
							GUIAttribute at = frm.getAttribute(nm);
							at.setBooleanValue(null);
							at.setType(GUIAttribute.TYPE_BOOLEAN);
						}
					} else
						frm.setValue(nm, val);
				} else {
					if (att != null) {
						if (att.getType() == GUIAttribute.TYPE_INT) {
							frm.getAttribute(nm).setIntValue(null);
							break;
						} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
							frm.getAttribute(nm).setBooleanValue(null);
							break;
						} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
							frm.getAttribute(nm).setDoubleValue(null);
							break;
						} else if (att.getType() == GUIAttribute.TYPE_DATE) {
							frm.getAttribute(nm).setDateValue(null);
							break;
						} else if (att.getType() == GUIAttribute.TYPE_USER) {
							GUIAttribute at = frm.getAttribute(nm);
							at.setIntValue(null);
							at.setStringValue(null);
							at.setType(GUIAttribute.TYPE_USER);
							break;
						} else {
							frm.setValue(nm, "");
							break;
						}
					}
				}
			}
		}

		ContactingServer.get().show();
		DocumentService.Instance.get().createEmpty(frm, new AsyncCallback<GUIDocument>() {
			@Override
			public void onFailure(Throwable caught) {
				ContactingServer.get().hide();
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocument doc) {
				ContactingServer.get().hide();
				DocumentsPanel.get().refresh();
				destroy();
			}
		});
	}
}