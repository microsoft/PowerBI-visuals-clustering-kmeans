/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
module powerbi.extensibility.visual {
    "use strict";
    // below is a snippet of a definition for an object which will contain the property values
    // selected by the users
    /*interface VisualSettings {
        lineColor: string;
    }*/

    // to allow this scenario you should first the following JSON definition to the capabilities.json file
    // under the "objects" property:
    // "settings": {
    //     "displayName": "Visual Settings",
    //     "description": "Visual Settings Tooltip",
    //     "properties": {
    //         "lineColor": {
    //         "displayName": "Line Color",
    //         "type": { "fill": { "solid": { "color": true }}}
    //         }
    //     }
    // }

    // in order to improve the performance, one can update the <head> only in the initial rendering.
    // set to 'true' if you are using different packages to create the widgets
    const updateHTMLHead: boolean = false;
    const renderVisualUpdateType: number[] = [
        VisualUpdateType.Resize,
        VisualUpdateType.ResizeEnd,
        VisualUpdateType.Resize + VisualUpdateType.ResizeEnd
    ];

    export class Visual implements IVisual {
        private rootElement: HTMLElement;
        private headNodes: Node[];
        private bodyNodes: Node[];
        private settings: VisualSettings;

        public constructor(options: VisualConstructorOptions) {
            if (options && options.element) {
                this.rootElement = options.element;
            }
            this.headNodes = [];
            this.bodyNodes = [];
        }

        public update(options: VisualUpdateOptions): void {

            if (!options ||
                !options.type ||
                !options.viewport ||
                !options.dataViews ||
                options.dataViews.length === 0 ||
                !options.dataViews[0]) {
                return;
            }
            const dataView: DataView = options.dataViews[0];
            this.settings = Visual.parseSettings(dataView);

            let payloadBase64: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                payloadBase64 = dataView.scriptResult.payloadBase64;
            }

            if (renderVisualUpdateType.indexOf(options.type) === -1) {
                if (payloadBase64) {
                    this.injectCodeFromPayload(payloadBase64);
                }
            } else {
                this.onResizing(options.viewport);
            }
        }

        public onResizing(finalViewport: IViewport): void {
            /* add code to handle resizing of the view port */
        }

        private injectCodeFromPayload(payloadBase64: string): void {
            // inject HTML from payload, created in R
            // the code is injected to the 'head' and 'body' sections.
            // if the visual was already rendered, the previous DOM elements are cleared

            ResetInjector();

            if (!payloadBase64) {
                return;
            }

            // create 'virtual' HTML, so parsing is easier
            let el: HTMLHtmlElement = document.createElement("html");
            try {
                el.innerHTML = window.atob(payloadBase64);
            } catch (err) {
                return;
            }

            // if 'updateHTMLHead == false', then the code updates the header data only on the 1st rendering
            // this option allows loading and parsing of large and recurring scripts only once.
            if (updateHTMLHead || this.headNodes.length === 0) {
                while (this.headNodes.length > 0) {
                    let tempNode: Node = this.headNodes.pop();
                    document.head.removeChild(tempNode);
                }
                let headList: NodeListOf<HTMLHeadElement> = el.getElementsByTagName("head");
                if (headList && headList.length > 0) {
                    let head: HTMLHeadElement = headList[0];
                    this.headNodes = ParseElement(head, document.head);
                }
            }

            // update 'body' nodes, under the rootElement
            while (this.bodyNodes.length > 0) {
                let tempNode: Node = this.bodyNodes.pop();
                this.rootElement.removeChild(tempNode);
            }
            let bodyList: NodeListOf<HTMLBodyElement> = el.getElementsByTagName("body");
            if (bodyList && bodyList.length > 0) {
                let body: HTMLBodyElement = bodyList[0];
                this.bodyNodes = ParseElement(body, this.rootElement);
            }

            RunHTMLWidgetRenderer();
        }

        private static parseSettings(dataView: DataView): VisualSettings {
            return VisualSettings.parse(dataView) as VisualSettings;
        }

        /** 
        * This function gets called for each of the objects defined in the capabilities files and allows you to select which of the 
        * objects and properties you want to expose to the users in the property pane.
        * 
        */
        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch (objectName) {
                case 'settings_prepocessing_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            scaleData: this.settings.settings_prepocessing_params.scaleData,
                            applyPCA: this.settings.settings_prepocessing_params.applyPCA

                        },
                        selector: null
                    });
                    break;

                case 'settings_clusterNum_params':
                    if (this.settings.settings_clusterNum_params.numOfClusters === "auto") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                numOfClusters: this.settings.settings_clusterNum_params.numOfClusters,
                                numClustersMethods: ifStringReturnStringClustersMethod(this.settings.settings_clusterNum_params.numClustersMethods, this.settings.settings_clusterNum_params.numOfClusters)
                            },
                            selector: null
                        });
                    }
                    else {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                //  show: this.settings.settings_clusterNum_params.show,
                                numOfClusters: this.settings.settings_clusterNum_params.numOfClusters,
                            },
                            selector: null
                        });
                    }
                    break;



                case 'settings_viz_params':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            // show: this.settings.settings_viz_params.show,
                            percentile: this.settings.settings_viz_params.percentile,
                            weight: this.settings.settings_viz_params.weight,
                            drawEllipse: this.settings.settings_viz_params.drawEllipse,
                            drawConvexHull: this.settings.settings_viz_params.drawConvexHull
                            // drawCentroid: this.settings.settings_viz_params.drawCentroid,

                        },
                        selector: null
                    });
                    if (!this.settings.settings_viz_params.drawEllipse) {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                drawCentroid: this.settings.settings_viz_params.drawCentroid
                            },
                            selector: null
                        });
                    }

                    break;
                case 'settings_labeling_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings.settings_labeling_params.show,
                            textSize: this.settings.settings_labeling_params.textSize,
                            percentile: this.settings.settings_labeling_params.percentile,
                            maxLenPointLabel: inMinMax(this.settings.settings_labeling_params.maxLenPointLabel, 1, 100),
                            percentile1: inMinMax(this.settings.settings_labeling_params.percentile1, 0, 100)
                        },
                        selector: null
                    });
                    break;
                case 'settings_representative_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings.settings_representative_params.show,
                            textSize: this.settings.settings_representative_params.textSize,
                            maxLenDelegateLabel: inMinMax(this.settings.settings_representative_params.maxLenDelegateLabel, 1, 100),
                        },
                        selector: null
                    });
                    break;
                case 'settings_legend_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings.settings_legend_params.show,
                            addLegend: this.settings.settings_legend_params.addLegend,
                            palleteType: this.settings.settings_legend_params.palleteType
                        },
                        selector: null
                    });
                    break;

                case 'settings_additional_params':
                    if (this.settings.settings_clusterNum_params.numOfClusters === "auto") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                minClusters: inMinMax(this.settings.settings_additional_params.minClusters, 1, 15),
                                maxClusters: inMinMax(this.settings.settings_additional_params.maxClusters, this.settings.settings_additional_params.minClusters, 15),
                                maxIter: inMinMax(this.settings.settings_additional_params.maxIter, 1, 100),
                                nStart: inMinMax(this.settings.settings_additional_params.nStart, 1, 100),
                                sparsify: this.settings.settings_additional_params.sparsify
                            },
                            selector: null
                        });
                    }
                    else {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                // show: this.settings.settings_additional_params.show,
                                maxIter: inMinMax(this.settings.settings_additional_params.maxIter, 1, 100),
                                nStart: inMinMax(this.settings.settings_additional_params.nStart, 1, 100),
                                sparsify: this.settings.settings_additional_params.sparsify
                            },
                            selector: null
                        });
                    }
                    break;
                case 'settings_export_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings.settings_export_params.show,
                            limitExportSize: this.settings.settings_export_params.limitExportSize,
                            method: this.settings.settings_export_params.method
                        },
                        selector: null
                    });
                    break;
            };
            return objectEnumeration;
        }
    }
}