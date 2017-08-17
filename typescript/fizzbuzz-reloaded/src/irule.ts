export interface IRule {
    appliesTo(input: number): boolean;
    resultFor(input: number): string;
}
