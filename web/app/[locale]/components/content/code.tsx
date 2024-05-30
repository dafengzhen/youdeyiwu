import clsx from 'clsx';

export default function Code({
  classs,
  value,
}: {
  classs?: string;
  value: string;
}) {
  function handleLineHighlight(code: string) {
    const nums = rangeParser(code, extractCodeRange(code));
    return code
      .split('\n')
      .reduce((pValue, cValue, cIndex) => {
        let node;
        if (nums.includes(cIndex)) {
          node = cValue.startsWith('<span class="line">')
            ? cValue.replace(
                '<span class="line">',
                '<span class="line line-highlight bg-secondary-subtle">',
              )
            : cValue;
        } else {
          node = cValue;
        }
        pValue.push(node);
        return pValue;
      }, [] as string[])
      .filter((value) => {
        const _value = value.trim();
        return !(_value.includes('// h-start') || _value.includes('// h-end'));
      })
      .map((value, index) => {
        const regex = /<span class="[^"]*">(\d+)<\/span>/;
        if (regex.test(value)) {
          return value.replace(
            regex,
            `<span class="user-select-none d-inline-block align-middle text-truncate w-2 me-2 text-secondary text-opacity-75">${index + 1}</span>`,
          );
        }
        return value;
      })
      .join('\n');
  }

  function extractCodeRange(code: string) {
    const regex = /\/\/\s*h-start([\s\S]*?)\/\/\s*h-end/g;
    const match = regex.exec(code);
    if (match) {
      return match[1];
    }
  }

  function rangeParser(code: string, rangeCode: string | undefined) {
    const nums: number[] = [];
    const codeLines = code.split('\n').map((value) => value.trim());
    (rangeCode ?? '').split('\n').forEach((value) => {
      const index = codeLines.indexOf(value.trim());
      if (index !== -1) {
        nums.push(index);
      }
    });
    return nums;
  }

  function handleLines(code: string) {
    const strings = code.split('\n');
    if (strings.length === 1) {
      return strings.join('\n');
    }

    return strings
      .reduce((pValue, cValue, cIndex) => {
        const node =
          '<span class="line">' +
          `<span class="user-select-none d-inline-block align-middle text-truncate w-2 me-2 text-secondary text-opacity-75">${cIndex + 1}</span>` +
          cValue +
          '</span>';
        pValue.push(node);
        return pValue;
      }, [] as string[])
      .join('\n');
  }

  return (
    <pre>
      <code
        className={clsx(classs, 'hljs')}
        dangerouslySetInnerHTML={{
          __html: handleLineHighlight(handleLines(value)),
        }}
      ></code>
    </pre>
  );
}
