export class Result {
    public roman: string;
    public arabic: number;

    constructor(arabicNumber: number) {
        this.arabic = arabicNumber;
        this.roman = "";
    }
}
