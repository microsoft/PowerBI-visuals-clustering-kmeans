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

    export interface ScriptResult {
        source: string;
        provider: string;
    }
    
    interface VisualSettings1 {//data preprocessing
        show: boolean;
        scaleData: boolean;      
        applyPCA: boolean;            
    }

     interface VisualSettings2 {//clustering algo
        show: boolean;
        numOfClusters: string;
        numClustersMethods: string;

     }

     interface VisualSettings3 {//appearance 
        show: boolean;
        drawEllipse: boolean;
        drawConvexHull: boolean;
        drawCentroid: boolean;
        percentile: number; //TODO: percentage
        weight: number;
       
     }
     interface VisualSettings4 {//points labeling 
        show: boolean;
         //addLabel2points: boolean;
         textSize: number; //TODO: textSize
         //cexLabel2points: number; //TODO: textSize
         percentile: number; //TODO: percentage
         maxLenPointLabel: number;
         percentile1: number; //TODO: percentage

     }
      interface VisualSettings5 {//dalegate labeling 
        show: boolean;
        //addLabel2clusterDelegate: boolean;   
        textSize: number; //TODO: textSize
        maxLenDelegateLabel: number;

     }
     interface VisualSettings6 {//legend and pallete 
        show: boolean;
        addLegend: boolean;
        palleteType: string; //TODO

     }
     interface VisualSettings7 {//additional settings 
        show: boolean;
         showWarnings: boolean;
         minClusters: number;
         maxClusters: number;
         maxIter: number;
         nStart: number;
     }

    

    export class Visual implements IVisual {
        private imageDiv: HTMLDivElement;
        private imageElement: HTMLImageElement;

        private settings1: VisualSettings1;
        private settings2: VisualSettings2;
        private settings3: VisualSettings3;
        private settings4: VisualSettings4;
        private settings5: VisualSettings5;
        private settings6: VisualSettings6;
        private settings7: VisualSettings7;
      

        public constructor(options: VisualConstructorOptions) {
            this.imageDiv = document.createElement('div');
            this.imageDiv.className = 'rcv_autoScaleImageContainer';
            options.element.appendChild(this.imageDiv);
            
            this.imageElement = document.createElement('img');
            this.imageElement.className = 'rcv_autoScaleImage';

            this.imageDiv.appendChild(this.imageElement);

             this.settings1 = <VisualSettings1>{
                show: false,
                 scaleData: false,
                 applyPCA: false,
            };
            this.settings2 = <VisualSettings2>{
                show: false,
                numOfClusters: "auto",
                numClustersMethods: "fast",
            };
            this.settings3 = <VisualSettings3>{
                show: false,
                drawEllipse: false,
                drawConvexHull: false,
                drawCentroid: false,
                percentile: 40,
                weight: 10,
            };
            this.settings4 = <VisualSettings4>{
                show: true,
                textSize: 8, 
                percentile: 80, 
                maxLenPointLabel: 5,
                percentile1: 100 
            };
            this.settings5 = <VisualSettings5>{
                show: false,
                textSize: 8, 
                maxLenDelegateLabel: 30
                
            };
            this.settings6 = <VisualSettings6>{
                show: true,
                addLegend: true,
                palleteType: "rainbow"
                
            };
            this.settings7 = <VisualSettings7>{
                show: false,
                showWarnings: true,
                minClusters: 2,
                maxClusters: 12,
                maxIter: 10,
                nStart: 5,
            };
            
        }

        public update(options: VisualUpdateOptions) {
            let dataViews: DataView[] = options.dataViews;
            if (!dataViews || dataViews.length === 0)
                return;

            let dataView: DataView = dataViews[0];
            if (!dataView || !dataView.metadata)
                return;

            this.settings1 = <VisualSettings1> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings1', 'show', false),
                scaleData: getValue<boolean>(dataView.metadata.objects, 'settings1', 'scaleData', false),              
                applyPCA: getValue<boolean>(dataView.metadata.objects, 'settings1', 'applyPCA',false),
            };

            this.settings2= <VisualSettings2> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings2', 'show', false),
                numOfClusters: getValue<string>(dataView.metadata.objects, 'settings2', 'numOfClusters',"auto"),
                numClustersMethods: getValue<string>(dataView.metadata.objects, 'settings2', 'numClustersMethods',"fast"),
            };
            this.settings3 = <VisualSettings3> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings3', 'show', false),
                drawEllipse: getValue<boolean>(dataView.metadata.objects, 'settings3', 'drawEllipse', false),
                drawConvexHull: getValue<boolean>(dataView.metadata.objects, 'settings3', 'drawConvexHull', false),
                drawCentroid: getValue<boolean>(dataView.metadata.objects, 'settings3', 'drawCentroid', false),
                percentile: getValue<number>(dataView.metadata.objects, 'settings3', 'percentile',40),
                weight: getValue<number>(dataView.metadata.objects, 'settings3', 'weight',10),
            };

            this.settings4 = <VisualSettings4> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings4', 'show', true),
               //addLabel2points: getValue<boolean>(dataView.metadata.objects, 'settings4', 'addLabel2points',false),
                textSize: getValue<number>(dataView.metadata.objects, 'settings4', 'textSize',8),
                percentile: getValue<number>(dataView.metadata.objects, 'settings4', 'percentile',100),
                maxLenPointLabel: getValue<number>(dataView.metadata.objects, 'settings4', 'maxLenPointLabel',5),
                percentile1: getValue<number>(dataView.metadata.objects, 'settings4', 'percentile1',100),
            };
            this.settings5 = <VisualSettings5> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings5', 'show', false),
                //addLabel2clusterDelegate: getValue<boolean>(dataView.metadata.objects, 'settings5', 'addLabel2clusterDelegate', false),  
                textSize: getValue<number>(dataView.metadata.objects, 'settings5', 'textSize', 8),
                maxLenDelegateLabel: getValue<number>(dataView.metadata.objects, 'settings5', 'maxLenDelegateLabel', 30)

            };
            this.settings6 = <VisualSettings6> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings6', 'show', true),
                addLegend: getValue<boolean>(dataView.metadata.objects, 'settings6', 'addLegend', true),
                palleteType: getValue<string>(dataView.metadata.objects, 'settings6', 'palleteType', "rainbow"),
            };

            this.settings7 = <VisualSettings7> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings7', 'show', false),
                showWarnings: getValue<boolean>(dataView.metadata.objects, 'settings7', 'showWarnings', true),
                minClusters: getValue<number>(dataView.metadata.objects, 'settings7', 'minClusters', 2),
                maxClusters: getValue<number>(dataView.metadata.objects, 'settings7', 'maxClusters', 12),
                maxIter: getValue<number>(dataView.metadata.objects, 'settings7', 'maxIter', 10),
                nStart: getValue<number>(dataView.metadata.objects, 'settings7', 'nStart', 5),
            };
      
           

            let imageUrl: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                imageUrl = "data:image/png;base64," + dataView.scriptResult.payloadBase64;
            }

            if (imageUrl) {
                this.imageElement.src = imageUrl;
            } else {
                this.imageElement.src = null;
            }

            this.onResizing(options.viewport);
        }

        public onResizing(finalViewport: IViewport): void {
            this.imageDiv.style.height = finalViewport.height + 'px';
            this.imageDiv.style.width = finalViewport.width + 'px';
        }

        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch(objectName) {
                case 'settings1':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings1.show,
                            scaleData: this.settings1.scaleData,
                            applyPCA: this.settings1.applyPCA,
                            
                         },
                        selector: null
                    });
                    break;
                    
                    case 'settings2':
                    if(this.settings2.numOfClusters=="auto")
                    {
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings2.show,
                            numOfClusters: this.settings2.numOfClusters,
                            numClustersMethods:ifStringReturnStringClustersMethod(this.settings2.numClustersMethods, this.settings2.numOfClusters)
                         },
                        selector: null
                    });
                    }
                    else
                    {
                       objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings2.show,
                            numOfClusters: this.settings2.numOfClusters,
                         },
                        selector: null
                    });


                    }
                    break;
                    
                    

                    case 'settings3':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings3.show,
                            drawEllipse: this.settings3.drawEllipse,
                            drawConvexHull: this.settings3.drawConvexHull,
                            drawCentroid: this.settings3.drawCentroid,
                            percentile: this.settings3.percentile,
                            weight: this.settings3.weight
                         },
                        selector: null
                    });
                    break;
                    case 'settings4':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings4.show,                   
                            textSize: this.settings4.textSize,
                            percentile: this.settings4.percentile,
                            maxLenPointLabel: this.settings4.maxLenPointLabel,
                            percentile1: inMinMax(this.settings4.percentile1, 0, 100)
                         },
                        selector: null
                    });
                    break;
                    case 'settings5':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings5.show,
                            //addLabel2clusterDelegate: this.settings5.addLabel2clusterDelegate,
                            textSize: this.settings5.textSize,
                            maxLenDelegateLabel: this.settings5.maxLenDelegateLabel,
                         },
                        selector: null
                    });
                    break;
                    case 'settings6':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings6.show,
                            addLegend: this.settings6.addLegend,
                            palleteType: this.settings6.palleteType
                         },
                        selector: null
                    });
                    break;
                    case 'settings7':
                     if(this.settings2.numOfClusters=="auto")
                    {
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings7.show,
                            showWarnings: this.settings7.showWarnings,
                          
                            minClusters: inMinMax(this.settings7.minClusters, 1, 15),
                            maxClusters: inMinMax(this.settings7.maxClusters, this.settings7.minClusters, 15),
                          
                            maxIter: inMinMax(this.settings7.maxIter, 1, 100),
                             nStart: inMinMax(this.settings7.nStart, 1, 100),
                         },
                        selector: null
                    });
                    }
                    else
                    {
                      objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings7.show,
                            showWarnings: this.settings7.showWarnings,   
                            maxIter: inMinMax(this.settings7.maxIter, 1, 100),
                             nStart: inMinMax(this.settings7.nStart, 1, 100),
                         },
                        selector: null
                    });  
                    }
                    break;
                    
            };

            return objectEnumeration;
        }
    }
}