/*
 *  Power BI Visualizations
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

    import DataViewObjectsParser = powerbi.extensibility.utils.dataview.DataViewObjectsParser;


  // returns value in range 
  export function inMinMax(aNumber: number, minNumber: number, maxNumber: number)
  {
      if(aNumber < minNumber)
          return minNumber;
      if(aNumber > maxNumber)
          return maxNumber;
      return aNumber;
  }

// update param of clustering based on another param
export function ifStringReturnStringClustersMethod(numClustersMethods:string , numOfClusters:string)
{
      if(numOfClusters!="auto")
          return "None"
      
      if(numOfClusters=="auto" && numClustersMethods=="None")
          return "fast"

      return numClustersMethods;
}



   export class VisualSettings extends DataViewObjectsParser {
      //public rcv_script: rcv_scriptSettings = new rcv_scriptSettings();
      public settings_prepocessing_params: settings_prepocessing_params = new settings_prepocessing_params();
      public settings_clusterNum_params: settings_clusterNum_params = new settings_clusterNum_params();
      public settings_viz_params: settings_viz_params = new settings_viz_params();
      public settings_labeling_params: settings_labeling_params = new settings_labeling_params();
      public settings_representative_params: settings_representative_params = new settings_representative_params();
      public settings_legend_params: settings_legend_params = new settings_legend_params();
      public settings_additional_params: settings_additional_params = new settings_additional_params();
      public settings_export_params: settings_export_params = new settings_export_params();
     
      }

      export class settings_prepocessing_params {
        public scaleData: boolean =  false;
        public applyPCA: boolean =  false;

      }
      export class settings_clusterNum_params {
        public numOfClusters: string =  "auto";
        public numClustersMethods: string =  "fast";
      }
      export class settings_viz_params {
        public drawEllipse: boolean =  false;
        public drawConvexHull: boolean =  false;
        public drawCentroid: boolean =  false;
        public percentile: number =  40;
        public weight: number =  10;
      }
      export class settings_labeling_params {
        public show: boolean =  true;
        public  textSize: number =  8;
        public  percentile: number =  80;
        public  maxLenPointLabel: number =  5;
        public  percentile1: number =  100;
      }
      export class settings_representative_params {
        public  show: boolean =  false;
        public textSize: number =  8;
        public maxLenDelegateLabel: number =  30;
      }
      export class settings_legend_params {
        public show: boolean =  true;
        public  addLegend: boolean =  true;
        public  palleteType: string =  "qPBI";
      }
      export class settings_additional_params {
        public  sparsify: boolean =  true;
        public minClusters: number =  2;
        public maxClusters: number =  12;
        public maxIter: number =  10;
        public nStart: number =  5;
      }
      export class settings_export_params {
        public show: boolean =  false;
        public limitExportSize: string =  "10000";
        public method: string = "copy";
      }
    // export class rcv_scriptSettings {
    //  // undefined
    //   public provider     // undefined
    //   public source     }

}
