/*============================================================
    "Simplicity" theme programmatic settings
    Copyright 2003 and beyond, Isomorphic Software
============================================================*/

isc.loadSkin = function (theWindow) {
    if (theWindow == null) theWindow = window;
    with (theWindow) {

        isc.Page.setSkinDir("[ISOMORPHIC]/skins/Simplicity/");
        isc.Page.loadStyleSheet("[SKIN]/skin_styles.css", theWindow);

        isc.Class.modifyFrameworkStart();

        isc.Canvas.setProperties({
            // use synthetic scrollbars 
            // in mobile 
            // for Macs, since from OSX Lion onward, scrollbars are not shown by default
            // in Webkit browsers as css scrollbars are natively shown in some cases when 
            // this should not be necessary
            showCustomScrollbars:(isc.Browser.isMobile || isc.Browser.isMac ||
                                !(isc.Browser.isIE || isc.Browser.isFirefox)),
            groupBorderCSS :"1px solid #165fa7"
        });

        if(isc.Browser.isIE && isc.Browser.version >= 7 && !isc.Browser.isIE9) {
            isc.Canvas.setAllowExternalFilters(false);
            isc.Canvas.setNeverUseFilters(true);
            if(isc.Window) {
              isc.Window.addProperties({
                    modalMaskOpacity:null,
                    modalMaskStyle:"normal"
                });
                isc.Window.changeDefaults("modalMaskDefaults", { src : "[SKIN]opacity.png" });
            }
        }

        if(isc.RPCManager) {
            isc.RPCManager.addClassProperties({ promptStyle:"cursor" });
        }
        isc.Button.addProperties({
            height: 23
        });

        // define IButton so examples that support the new SmartClient skin image-based
        // button will fall back on the CSS-based Button with this skin
        isc.ClassFactory.defineClass("IButton", "Button");
        isc.ClassFactory.defineClass("IAutoFitButton", "AutoFitButton");
        if (isc.IButton.markAsFrameworkClass != null) isc.IButton.markAsFrameworkClass();
        if (isc.IAutoFitButton.markAsFrameworkClass != null) isc.IAutoFitButton.markAsFrameworkClass();

        isc.ClassFactory.defineClass("HeaderMenuButton", "IButton").addProperties({
            baseStyle: "headerButton"
        });

        // Have IMenuButton be just a synonym for IMenuButton
        if (isc.MenuButton) {
            isc.ClassFactory.overwriteClass("IMenuButton", "MenuButton");
            if (isc.IMenuButton.markAsFrameworkClass != null) isc.IMenuButton.markAsFrameworkClass();
            isc.MenuButton.addProperties({
                // copy the header (.button) background-color to match when sort arrow is hidden
                baseStyle : "button"
            });
        }

        if (isc.IconButton) {
            isc.IconButton.addProperties({
                menuIconSrc: "[SKINIMG]/Menu/submenu_down.gif"
            });
        }

        if (isc.PickTreeItem) {
            isc.overwriteClass("IPickTreeItem", "PickTreeItem");
        }

        isc.Label.addProperties({
            showFocused: false
        });

        //----------------------------------------
        // 1) Scrollbars
        //----------------------------------------
        // NOTE: not used by default in Simplicity
        isc.SimpleScrollThumb.addProperties({
            baseStyle:"scrollThumb",
            hSrc:"[SKIN]hthumb_grip.png",
            vSrc:"[SKIN]vthumb_grip.png"
        });

        isc.Scrollbar.addProperties({
            baseStyle:"scrollbar",
            btnSize:18,
            hSrc:"[SKIN]hscroll.png",
            hThumbClass:isc.HSimpleScrollThumb,
            showRollOver:true,
            thumbInset:0,
            thumbMinSize:20,
            thumbOverlap:2,
            vSrc:"[SKIN]vscroll.png",
            vThumbClass:isc.VSimpleScrollThumb
        });


        //----------------------------------------
        // 3) Resizebars
        //----------------------------------------
        // StretchImgSplitbar class renders as resize bar
        isc.StretchImgSplitbar.addProperties({
            capSize:10,
            showGrip:true,
            showOver : false
        });

        isc.Snapbar.addProperties({
            vSrc:"[SKIN]vsplit.gif",
            hSrc:"[SKIN]hsplit.gif",
            baseStyle:"splitbar",
            items : [
                {name:"blank", width:"capSize", height:"capSize"},
                {name:"blank", width:"*", height:"*"},
                {name:"blank", width:"capSize", height:"capSize"}
            ],
            showDownGrip:false,
            gripBreadth:5,
            gripLength:35,
            capSize:0,
            showRollOver : false,
            showDown : false
        });

        isc.Layout.addProperties({
            resizeBarSize:9,
            // Use the Snapbar as a resizeBar by default - subclass of Splitbar that
            // shows interactive (closed/open) grip images
            // Other options include the Splitbar, StretchImgSplitbar or ImgSplitbar
            resizeBarClass:"Snapbar"
        })

        if (isc.SectionItem) {
            isc.SectionItem.addProperties({
                height:26
            });
        }
        if (isc.SectionStack) {
            isc.SectionStack.addProperties({
                headerHeight:26
            });
        }

        if (isc.MiniNavControl) {
            isc.MiniNavControl.addProperties({
                src: isc.Browser.isIPhone ? "[SKIN]/miniNav.svg" : "[SKIN]/miniNav~2.png",
                showDisabled: true,
                showDown: false,
                upButtonSrc: null,
                downButtonSrc: null
            });
        }
        if (isc.NavigationBar) {
            isc.NavigationBar.addProperties({
                leftButtonIcon: "[SKINIMG]NavigationBar/back_arrow~2.png"
            });
            isc.NavigationBar.changeDefaults("leftButtonDefaults", {
                iconWidth: 14,
                iconHeight: 24,
                iconSpacing: 7,
                showDown: false,
                showRTLIcon: true
            });
            isc.NavigationBar.changeDefaults("titleLabelDefaults", {
                margin: 0
            });

            if (isc.Browser.isIPhone || isc.Browser.isIPad) {
                isc.NavigationBar.addProperties({
                    leftButtonIcon: "[SKINIMG]NavigationBar/back_arrow.svg"
                });
            }
        }
        if (isc.NavigationButton) {
            isc.NavigationButton.addProperties({
                padding: 0
            });
        }

        if (isc.HiliteRule) {
            isc.HiliteRule.changeDefaults("hiliteFormDefaults", {
                colWidths: [60, 60, 60, 60, 60, 43]
            });
        }

        if (isc.Menu) {
            isc.Menu.addProperties({
                styleName:"menuBorder",
                bodyStyleName:"menuMain"
            });
        }

        if (isc.ListGrid) {
            isc.ListGrid.addProperties({
                alternateRecordStyles : true,
                alternateBodyStyleName : null,
                editFailedCSSText:"color:FF6347;",
                errorIconSrc : "[SKINIMG]actions/exclamation.png",
                tallBaseStyle: "tallCell",
                backgroundColor:"#e7e7e7",
                headerBackgroundColor:null,
                expansionFieldImageWidth : 16,
                expansionFieldImageHeight : 16,
                headerBaseStyle : "headerButton",
                headerHeight:25,
                cellHeight:22,
                summaryRowHeight: 24, // should be cellHeight + top/bottom borders
                normalCellHeight:22,
                showHeaderMenuButton:true,
                headerMenuButtonConstructor:"HeaderMenuButton",
                headerMenuButtonWidth:17,

                groupLeadingIndent : 1,
                groupIconPadding : 3,
                groupIcon: "[SKINIMG]/ListGrid/group.gif",

                summaryRowStyle:"gridSummaryCell",
                groupSummaryStyle:"groupSummaryCell",


                expansionFieldTrueImage : "[SKINIMG]/ListGrid/row_expanded.gif",
                expansionFieldFalseImage: "[SKINIMG]/ListGrid/row_collapsed.gif",
                checkboxFieldImageWidth : 13,
                checkboxFieldImageHeight : 13
            });
            isc.ListGrid.changeDefaults("summaryRowDefaults", {
                bodyBackgroundColor:null,
                bodyStyleName:"summaryRowBody"
            });

        }

        if (isc.TreeGrid) {
            isc.TreeGrid.addProperties({
                alternateRecordStyles : false,
                tallBaseStyle: "treeTallCell",
                normalBaseStyle: "treeCell",
                applyRowNumberStyle:false,
                openerIconSize: 22,
                sortAscendingImage:{src:"[SKINIMG]ListGrid/sort_ascending.gif", width:7, height:7},
                sortDescendingImage:{src:"[SKINIMG]ListGrid/sort_descending.gif", width:7, height:7}
            })
        }

        if (isc.TabSet) {
            isc.TabSet.addProperties({
                useSimpleTabs : true,
                paneMargin:5,
                closeTabIcon:"[SKIN]/TabSet/close.gif",
                closeTabIconSize:11,
                scrollerSrc:"[SKIN]scroll.gif",
                pickerButtonSrc:"[SKIN]picker.gif",
                scrollerButtonSize:16,
                pickerButtonSize:16,
                tabBarThickness:24,
                iconOrientation:"right",
                showScrollerRollOver: false
            });

            // In Netscape Navigator 4.7x, set the backgroundColor directly since the css
            // background colors are not reliable
            if (isc.Browser.isNav) {
                isc.TabSet.addProperties({paneContainerDefaults:{backgroundColor:"#FFFFFF"}});
            }

            isc.TabBar.addProperties({
                leadingMargin:5,
                membersMargin:2,

                // keep the tabs from reaching the curved edge of the pane (regardless of align)
                layoutStartMargin:5,
                layoutEndMargin:5,

                styleName:"tabBar",
                leftStyleName:"tabBarLeft",
                topStyleName:"tabBarTop",
                rightStyleName:"tabBarRight",
                bottomStyleName:"tabBarBottom",

                baseLineConstructor:"Canvas",
                baseLineProperties : {
                    backgroundColor: "#C0C3C7",
                    overflow:"hidden"
                },
                baseLineThickness:1
            });
        }

        if (isc.ImgTab) isc.ImgTab.addProperties({capSize:7});

        if (isc.Window) {
            isc.Window.addProperties({
                showHeaderBackground: false,
                backgroundColor:"#FEFEFE",
                showFooter:false,
                membersMargin : 0,
                modalMaskOpacity : 10
            });
            isc.Window.changeDefaults("headerDefaults", {
                height:25,
                layoutMargin:0,
                membersMargin:0
            });
            isc.Window.changeDefaults("resizerDefaults", {
                src:"[SKIN]/Window/resizer.gif"
            });
            isc.Window.changeDefaults("headerIconDefaults", {
                width:15,
                height:15,
                src:"[SKIN]/Window/headerIcon.gif"
            });
            isc.Window.changeDefaults("restoreButtonDefaults", {
                src:"[SKIN]/headerIcons/cascade.gif",
                showRollOver:true,
                showDown:false,
                width:15,
                height:15
            });
            isc.Window.changeDefaults("closeButtonDefaults", {
                src:"[SKIN]/headerIcons/close.gif",
                showRollOver:true,
                showDown:false,
                width:15,
                height:15
            });
            isc.Window.changeDefaults("maximizeButtonDefaults", {
                src:"[SKIN]/headerIcons/maximize.gif",
                showRollOver:true,
                showDown:false,
                width:15,
                height:15
            });
            isc.Window.changeDefaults("minimizeButtonDefaults", {
                src:"[SKIN]/headerIcons/minimize.gif",
                showRollOver:true,
                showDown:false,
                width:15,
                height:15
            });
            isc.Window.changeDefaults("toolbarDefaults", {
                buttonConstructor: "IButton"
            }) ;

            if (isc.ColorPicker) {
                isc.ColorPicker.addProperties({
                    layoutMargin:2
                })
            }
        }

        if (isc.Dialog) {
            isc.Dialog.addProperties({
                bodyColor: "#f6f6f6"
            });
        }
        if (isc.Dialog.Warn) {
            if (isc.Browser.isTouch) isc.Dialog.Warn.showModalMask = true;
        }
        if (isc.Dialog.Prompt) {
            if (isc.Browser.isTouch) isc.Dialog.Prompt.showModalMask = true;
        }

        // Dynamic form skinning
        if (isc.FormItem) {
            isc.FormItem.addProperties({
                defaultIconSrc:"[SKIN]/DynamicForm/default_formItem_icon.gif",
                errorIconSrc : "[SKINIMG]actions/exclamation.png",
                iconHeight:18,
                iconWidth:18,
                iconVAlign:"middle"
            });
        }
        if (isc.TextItem) {
            isc.TextItem.addProperties({
                height:22,
                showFocused: true
            });
        }

        if (isc.TextAreaItem) {
            isc.TextAreaItem.addProperties({
                showFocused: true
            });
        }

        if (isc.SelectItem) {
            isc.SelectItem.addProperties({
                textBoxStyle:"selectItemText",
                showFocusedPickerIcon:false,
                pickerIconSrc:"[SKIN]/pickers/comboBoxPicker.gif",
                height:22,
                pickerIconWidth:18,
                valueIconSize:16
            });
        }

        if (isc.ComboBoxItem) {
            isc.ComboBoxItem.addProperties({
                textBoxStyle:"selectItemText",
                pendingTextBoxStyle:"comboBoxItemPendingText",
                showFocusedPickerIcon:false,
                pickerIconSrc:"[SKIN]/pickers/comboBoxPicker.gif",
                height:22,
                pickerIconWidth:18
            });
        }

        // used by SelectItem and ComboBoxItem for picklist
        if (isc.ScrollingMenu) {
            isc.ScrollingMenu.addProperties({
                showShadow:false,
                shadowDepth:5
            });
        }
        if (isc.DateItem) {
            isc.DateItem.addProperties({
                height:22,
                pickerIconWidth:16,
                pickerIconHeight:14,
                pickerIconSrc:"[SKIN]/DynamicForm/date_control.png"
            });
        }
        if (isc.ColorItem) {
            isc.ColorItem.addProperties({
                showEmptyPickerIcon: true
            });
        }
        if (isc.SpinnerItem) {
            isc.SpinnerItem.addProperties({
                textBoxStyle:"selectItemText",
                height:22
            });
            isc.SpinnerItem.changeDefaults("increaseIconDefaults", {
                width:16,
                height:11,
                showFocused:true,
                showDown:false,
                imgOnly:true,
                src:"[SKIN]/DynamicForm/spinner_control_increase.png"
            });
            isc.SpinnerItem.changeDefaults("decreaseIconDefaults", {
                width:16,
                height:11,
                showFocused:true,
                showDown:false,
                imgOnly:true,
                src:"[SKIN]/DynamicForm/spinner_control_decrease.png"
            });
            isc.SpinnerItem.changeDefaults("unstackedIncreaseIconDefaults", {
                src:"[SKIN]/DynamicForm/spinner_control_increase.png",
                showFocused:true,
                showDown:false
            });
            isc.SpinnerItem.changeDefaults("unstackedDecreaseIconDefaults", {
                src:"[SKIN]/DynamicForm/spinner_control_decrease.png",
                showFocused:true,
                showDown:false
            });
        }
        if (isc.PopUpTextAreaItem) {
            isc.PopUpTextAreaItem.addProperties({
                popUpIconSrc: "[SKIN]/DynamicForm/text_control.gif",
                popUpIconWidth:16,
                popUpIconHeight:16
            });
        }

        if (isc.ToolbarItem && isc.IAutoFitButton) {
            isc.ToolbarItem.addProperties({
                buttonConstructor:isc.IAutoFitButton,
                buttonProperties: {
                    autoFitDirection: isc.Canvas.BOTH
                }
            });
        }

        if (isc.DateRangeDialog) {
            isc.DateRangeDialog.changeDefaults("headerIconProperties", {
                src: "[SKIN]/DynamicForm/date_control.png"
            });
        }
        if (isc.MiniDateRangeItem) {
            isc.MiniDateRangeItem.addProperties({"pickerIconSrc": "[SKIN]/DynamicForm/date_control.png"});
        }
        if (isc.RelativeDateItem) {
            isc.RelativeDateItem.changeDefaults("pickerIconDefaults", {
                neverDisable: false,
                src: "[SKIN]/DynamicForm/date_control.png"
            });
        }

        if (isc.DateChooser) {
            isc.DateChooser.addProperties({
                showDoubleYearIcon:false,
                skinImgDir:"images/DateChooser/",
                headerStyle:"dateChooserButton",
                weekendHeaderStyle:"dateChooserWeekendButton",
                baseNavButtonStyle:"dateChooserNavButton",
                baseWeekdayStyle:"dateChooserWeekday",
                baseWeekendStyle:"dateChooserWeekend",
                baseBottomButtonStyle:"dateChooserBottomButton",
                alternateWeekStyles:false,
                todayButtonHeight:20,
                edgeCenterBackgroundColor:"#FFFFFF",
                backgroundColor:"#FFFFFF",
                border:"1px solid #868686"

            });
        }

        if (isc.ToolStrip) {
            isc.ToolStrip.addProperties({
                height:30,
                defaultLayoutAlign:"center"
            });
            isc.ToolStripResizer.addProperties({
                backgroundColor:"#f6f6f6"
            });

            isc.ToolStrip.changeDefaults("formWrapperDefaults",{cellPadding:3});
        }

        if (isc.ToolStripMenuButton) {
            
            isc.overwriteClass("ToolStripMenuButton", "MenuButton").addProperties({
                showTitle:false,
                showRollOver:true,
                showDown:true,
                labelVPad:0,
                //labelHPad:7,
                autoFit:true,
                baseStyle : "toolbarButton",
                height:22
            });
        }

        if (isc.ToolStripButton) {
            
            isc.overwriteClass("ToolStripButton", "Button").addProperties({
                showTitle:false,
                title:null,
                showRollOver:true,
                showDown:true,
                labelVPad:0,
                //labelHPad:7,
                autoFit:true,
                baseStyle : "toolbarButton",
                height:22
            });
        }

        // Default EdgedCanvas skinning (for any canvas where showEdges is set to true)
        if (isc.EdgedCanvas) {
            isc.EdgedCanvas.addProperties({
                edgeSize:6,
                edgeImage: "[SKINIMG]edges/edge.png"
            });
        }

        if (isc.Slider) {
            isc.Slider.addProperties({
                thumbThickWidth:17,
                thumbThinWidth:11,
                trackWidth:5,
                trackCapSize:2
            });
            isc.Slider.changeDefaults("rangeLabelDefaults", {
                showDisabled: true
            });
            isc.Slider.changeDefaults("valueLabelDefaults", {
                showDisabled: true
            });
            isc.Slider.changeDefaults("trackDefaults", {
                showDisabled: false
            });
        }

        if (isc.TileGrid) {
            isc.TileGrid.addProperties({
                valuesShowRollOver: true,
                styleName:null,
                showEdges:false
            });
        }

        if (isc.Calendar) {
            isc.Calendar.changeDefaults("datePickerButtonDefaults", {
                showDown:false,
                showOver : false,
                src:"[SKIN]/DynamicForm/date_control.png"
            });

            isc.Calendar.changeDefaults("controlsBarDefaults", {
                height:10,
                layoutBottomMargin :10
            });
            
            isc.Calendar.changeDefaults("eventCanvasCloseButtonDefaults", {
                src:"[SKIN]/headerIcons/close.gif"
            });
        }

        if (isc.Hover) {
            isc.addProperties(isc.Hover.hoverCanvasDefaults, {
                showShadow:false,
                shadowDepth:5
            })
        }

        //indicate type of media used for various icon types
        isc.pickerImgType = "gif";
        isc.transferImgType = "png";
        isc.headerImgType = "gif";

        // -------------------------------------------
        // Printing
        // -------------------------------------------
        if (isc.PrintWindow) {
            isc.PrintWindow.changeDefaults("printButtonDefaults", {
                height: 22
            });
        }

        if (isc.SplitPane) {
            isc.SplitPane.changeDefaults("backButtonDefaults", {
                icon: "[SKINIMG]NavigationBar/back_arrow~2.png",
                iconWidth: 14,
                iconHeight: 24,
                iconSpacing: 7,
                showDown: false,
                showRTLIcon: true
            });

            if (isc.Browser.isIPhone || isc.Browser.isIPad) {
                isc.SplitPane.changeDefaults("backButtonDefaults", {
                    icon: "[SKINIMG]NavigationBar/back_arrow.svg"
                });
            }
        }

        // remember the current skin so we can detect multiple skins being loaded
        if (isc.setCurrentSkin) isc.setCurrentSkin("Simplicity");

        isc.Page.checkBrowserAndRedirect("[SKIN]/unsupported_browser.html");

        isc.Class.modifyFrameworkDone();
    }
}


// call the loadSkin routine
isc.loadSkin();
