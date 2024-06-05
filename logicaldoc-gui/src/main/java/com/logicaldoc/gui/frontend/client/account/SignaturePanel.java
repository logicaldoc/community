package com.logicaldoc.gui.frontend.client.account;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.metadata.stamp.StampProperties;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays the signature stamp of a user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.5
 */
public class SignaturePanel extends StampProperties {

	public SignaturePanel(GUIStamp stamp) {
		super(stamp, null);
	}

	@Override
	protected void refresh() {
		vm.clearValues();
		vm.clearErrors(false);

		if (formsContainer.getMembers() != null)
			formsContainer.removeMembers(formsContainer.getMembers());

		if (formsContainer instanceof HLayout) {
			super.removeMember(formsContainer);
			super.formsContainer = new VLayout();
			setMembers(formsContainer);
		}

		vm.clearErrors(false);
		vm.clearValues();
		vm.resetValues();

		if (form1 != null)
			form1.destroy();

		form1 = new DynamicForm();
		form1.setNumCols(5);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setValuesManager(vm);

		TextItem exprx = ItemFactory.newTextItem("exprx", stamp.getExprX());
		if (changedHandler != null)
			exprx.addChangedHandler(changedHandler);
		exprx.setWidth(300);

		TextItem exprw = ItemFactory.newTextItem("exprw", stamp.getExprW());
		if (changedHandler != null)
			exprw.addChangedHandler(changedHandler);
		exprw.setWidth(300);

		TextItem expry = ItemFactory.newTextItem("expry", stamp.getExprY());
		if (changedHandler != null)
			expry.addChangedHandler(changedHandler);
		expry.setWidth(300);
		expry.setColSpan(4);

		TextItem exprh = ItemFactory.newTextItem("exprh", stamp.getExprH());
		if (changedHandler != null)
			exprh.addChangedHandler(changedHandler);
		exprh.setWidth(300);
		exprh.setColSpan(4);

		text = ItemFactory.newTextAreaItemForAutomation("text", stamp.getText(), changedHandler, false);
		text.setWidth("*");

		SpinnerItem rotation = ItemFactory.newSpinnerItem("rotation", stamp.getRotation(), 0, 360);
		if (changedHandler != null)
			rotation.addChangedHandler(changedHandler);

		final TextItem pageSelection = ItemFactory.newTextItem("pageSelection", I18N.message("sselection"),
				stamp.getPageSelection());
		pageSelection.setVisible(stamp.getPageOption() == GUIStamp.PAGE_OPT_SEL);
		if (changedHandler != null)
			pageSelection.addChangedHandler(changedHandler);

		final SelectItem pageOption = ItemFactory.newSelectItem("pageOption", "stampin");
		LinkedHashMap<String, String> pageOptions = new LinkedHashMap<>();
		pageOptions.put("" + GUIStamp.PAGE_OPT_ALL, I18N.message("allpages"));
		pageOptions.put("" + GUIStamp.PAGE_OPT_FIRST, I18N.message("firstpage"));
		pageOptions.put("" + GUIStamp.PAGE_OPT_LAST, I18N.message("lastpage"));
		pageOptions.put("" + GUIStamp.PAGE_OPT_SEL, I18N.message("selection"));
		pageOption.setValueMap(pageOptions);
		pageOption.setValue("" + stamp.getPageOption());
		pageOption.addChangedHandler(event -> {
			if (event.getValue().equals("" + GUIStamp.PAGE_OPT_SEL))
				pageSelection.show();
			else
				pageSelection.hide();
		});

		SpinnerItem opacity = ItemFactory.newSpinnerItem("opacity", stamp.getOpacity(), 1, 100);
		opacity.setEndRow(true);
		if (changedHandler != null)
			opacity.addChangedHandler(changedHandler);

		form1.setItems(pageOption, pageSelection, rotation, opacity, exprx, expry, exprw, exprh);

		/*
		 * For the spinners we need to manually update the VM or the widget will
		 * NOT be refreshed (unbelievable but that is)
		 */
		vm.setValue("rotation", stamp.getRotation());
		vm.setValue("opacity", stamp.getOpacity());
		vm.setValue("size", stamp.getSize());

		formsContainer.setMembers(image, form1);

		text.hide();
		image.show();
		refreshStampImage();
	}

	@Override
	protected void refreshStampImage() {
		if (image != null && formsContainer.contains(image)) {
			formsContainer.removeMember(image);
			image.destroy();
		}

		image = new VLayout();
		image.setAlign(VerticalAlignment.TOP);
		image.setMargin(1);
		image.setMembersMargin(2);


		Img imageItem = new Img(stampImageUrl(stamp.getId()));
		imageItem.setMaxWidth(300);
		image.setMembers(imageItem);
		formsContainer.addMember(image, 0);
	}
}
